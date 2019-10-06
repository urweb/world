open Json
open Urls

signature S = sig
    val authorize_url : url
    val access_token_url : url

    val client_id : string
    val client_secret : string
    val scope : option string

    val withToken : {Token : string, Expiration : option int} -> transaction unit
end

table states : { State : int, Expires : time }
  PRIMARY KEY State

task periodic 300 = fn () =>
                       dml (DELETE FROM states WHERE Expires < CURRENT_TIMESTAMP)


datatype access_response =
         Token of string
       | Error of string

type token_response = {Token : string, Expires : int}
val _ : json token_response = json_record {Token = "access_token", Expires = "expires_in"}

type error_response = {Error : string}
val _ : json error_response = json_record {Error = "error_description"}

functor Make(M : S) = struct
    open M

    fun authorize {ReturnTo = rt} =
        let
            fun authorized rt (qs : option queryString) =
                case qs of
                    None => error <xml>No query string in OAuth authorization response</xml>
                  | Some qs =>
                    let
                        fun parse1 s code state =
                            case String.split s #"=" of
                                None =>
                                (case code of
                                     None => error <xml>No code in OAuth authorization response</xml>
                                   | Some code =>
                                     case state of
                                         None => error <xml>No state in OAuth authorization response</xml>
                                       | Some state =>
                                         case read state of
                                             None => error <xml>Noninteger state in OAuth authorization response</xml>
                                           | Some state =>
                                             (urldecode code, state))
                              | Some (param, s) =>
                                let
                                    val (value, rest) =
                                        case String.split s #"&" of
                                            None => (s, "")
                                          | Some p => p
                                in
                                    parse1 rest
                                    (if param = "code" then Some value else code)
                                    (if param = "state" then Some value else state)
                                end

                        (* This parser assumes a URL-encoded query string. *)
                        fun parse2' s token expiry =
                            case String.split s #"=" of
                                None =>
                                (case token of
                                     None => None
                                   | Some token => Some (token, expiry))
                              | Some (param, s) =>
                                let
                                    val (value, rest) =
                                        case String.split s #"&" of
                                            None => (s, "")
                                          | Some p => p
                                in
                                    parse2' rest
                                    (case param of
                                         "access_token" => Some (Token (urldecode value))
                                       | "error_description" => Some (Error (urldecode value))
                                       | _ => token)
                                    (case param of
                                         "expires_in" =>
                                         (case read (urldecode value) of
                                              None => error <xml>Malformed expiration in OAuth response</xml>
                                            | Some n => Some n)
                                       | _ => expiry)
                                end

                        (* This parser first attempts the last one,
                         * falling back on JSON if inapplicable. *)
                        fun parse2 s =
                            case parse2' s None None of
                                None =>
                                (case String.split s #"\"" of
                                     None => error <xml>No token in OAuth access response</xml>
                                   | Some (_, rest) =>
                                     if String.isPrefix {Full = rest, Prefix = "error"} then
                                         (Error ((fromJson s : error_response).Error), None)
                                     else
                                         let
                                             val r = fromJson s : token_response
                                         in
                                             (Token r.Token, Some r.Expires)
                                         end)
                              | Some p => p

                        val (code, state) = parse1 (show qs) None None
                    in
                        goodState <- oneRowE1 (SELECT COUNT( * ) > 0
                                               FROM states
                                               WHERE states.State = {[state]});
                        if not goodState then
                            error <xml>Bad state in OAuth authorization response</xml>
                        else
                            dml (DELETE FROM states
                                 WHERE State = {[state]});
                            pb <- WorldFfi.post access_token_url
                                                ("client_id=" ^ urlencode client_id
                                                 ^ "&client_secret=" ^ urlencode client_secret
                                                 ^ "&code=" ^ urlencode code
                                                 ^ "&grant_type=authorization_code"
                                                 ^ "&redirect_uri=" ^ urlencode (show (effectfulUrl (authorized (show rt)))));
                            (token, expiry) <- return (parse2 pb);
                            case token of
                                Error msg => error <xml>OAuth error: {[msg]}</xml>
                              | Token token =>
                                withToken {Token = token, Expiration = expiry};
                                redirect (bless rt)
                    end
        in
            state <- rand;
            tm <- now;
            dml (INSERT INTO states(State, Expires) VALUES({[state]}, {[addSeconds tm 300]}));

            redirect (bless (show authorize_url
                             ^ "?client_id=" ^ urlencode client_id
                             ^ "&redirect_uri=" ^ urlencode (show (effectfulUrl (authorized (show rt))))
                             ^ "&state=" ^ show state
                             ^ "&response_type=code"
                             ^ (case scope of
                                    None => ""
                                  | Some scope => "&scope=" ^ scope)))
        end
end
