open Json
open Urls

type instance = string
val read_instance = _
val show_instance = _

functor ThreeLegged(M : sig
                        val https : bool
                        val sandbox : bool

                        val client_id : string
                        val client_secret : string
                    end) = struct
    open M

    table secrets : { Secret : int,
                      Token : string,
                      Expires : time }
      PRIMARY KEY Secret

    task periodic 60 = fn () =>
                          tm <- now;
                          dml (DELETE FROM secrets
                               WHERE Expires < {[addSeconds tm (-60)]})

    cookie user : int

    fun withToken {Token = tok, Expiration = seconds, ...} =
        seconds <- return (Option.get (10 * 60) seconds);
        secret <- rand;
        tm <- now;
        dml (INSERT INTO secrets(Secret, Token, Expires)
             VALUES ({[secret]}, {[tok]}, {[addSeconds tm (seconds * 3 / 4)]}));
        setCookie user {Value = secret,
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M
                        
                        val url_base = "https://" ^ (if sandbox then "test." else "login.") ^ "salesforce.com/services/oauth2/"
                        val authorize_url = bless (url_base ^ "authorize")
                        val access_token_url = bless (url_base ^ "token")
                        val scope = None

                        val withToken = withToken
                    end)

    val token =
        c <- getCookie user;
        case c of
            None => return None
          | Some n =>
            oneOrNoRowsE1 (SELECT (secrets.Token)
                           FROM secrets
                           WHERE secrets.Secret = {[n]}
                             AND secrets.Expires > CURRENT_TIMESTAMP)

    val loggedIn = c <- getCookie user;
        case c of
            None => return False
          | Some s =>
            expiresO <- oneOrNoRowsE1 (SELECT (secrets.Expires)
                                       FROM secrets
                                       WHERE secrets.Secret = {[s]});
            case expiresO of
                None => return False
              | Some exp =>
                tm <- now;
                return (tm < exp)
    val logout = clearCookie user

    fun auth url =
        authorize {ReturnTo = bless url}

    val status =
        li <- loggedIn;
        li <- source li;
        cur <- currentUrl;
        return <xml>
          <dyn signal={liV <- signal li;
                       if liV then
                           return <xml><button value="Log out of Salesforce"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into Salesforce"
                                               onclick={fn _ => redirect (url (auth (show cur)))}/></xml>}/>
        </xml>
end

type account = string
val read_account = _
val show_account = _

type query_result = {
     Nam : account
}
val _ : json query_result = json_record {Nam = "Name"}
                   
type query_results = {
     Records : list query_result
}
val _ : json query_results = json_record {Records = "records"}

type response = {
     Success : bool
}
val _ : json response = json_record {Success = "success"}
                             
functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Salesforce to use this feature.</xml>
          | Some tok => return tok
         
    fun api url =
        tok <- token;
        WorldFfi.get url (Some ("Bearer " ^ tok))

    fun apiPost url body =
        tok <- token;
        WorldFfi.post url (Some ("Bearer " ^ tok)) (Some "application/json") body

    fun apiPut url body =
        tok <- token;
        WorldFfi.put url (Some ("Bearer " ^ tok)) (Some "application/json") body

    fun apiDelete url =
        tok <- token;
        WorldFfi.delete url (Some ("Bearer " ^ tok))

    structure Accounts = struct
        fun list inst =
            s <- api (bless ("https://" ^ inst ^ ".salesforce.com/services/data/v47.0/query?q=SELECT+name+from+Account"));
            return (List.mp (fn r => r.Nam) (fromJson s : query_results).Records)

        fun exists inst name =
            s <- api (bless ("https://" ^ inst ^ ".salesforce.com/services/data/v47.0/query?q=SELECT+name+from+Account+where+name='" ^ urlencode name ^ "'"));
            return (List.length (fromJson s : query_results).Records = 1)

        fun insert inst name =
            s <- apiPost (bless ("https://" ^ inst ^ ".salesforce.com/services/data/v47.0/sobjects/Account/")) (toJson {Nam = name});
            if (fromJson s : response).Success then
                return ()
            else
                error <xml>Salesforce account insert failed.</xml>
    end
end
