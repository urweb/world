open Json

table secrets : { Email : string,
                  Nam : string,
                  Secret : int }
  PRIMARY KEY Email

cookie user : { Email : string, Secret : int }

signature S = sig
    val authorize_url : url
    val access_token_url : url
    val userinfo_url : url
    val client_id : string
    val client_secret : string
    val https : bool
    val onCompletion : transaction page
end

type claims = {
     Email : option string,
     Nam : option string,
     GivenName : option string,
     MiddleName : option string,
     FamilyName : option string,
     EmailVerified : option bool
}
val _ : json claims = json_record_withOptional {}
                      {Email = "email",
                       Nam = "name",
                       GivenName = "given_name",
                       MiddleName = "middle_name",
                       FamilyName = "family_name",
                       EmailVerified = "email_verified"}

fun concato (s1 : option string) (s2 : option string) =
    case s1 of
        None => s2
      | Some s1' =>
        case s2 of
            None => s1
          | Some s2' => Some (s1' ^ " " ^ s2')

functor Make(M : S) = struct
    open M

    fun withToken {Token = tok, ...} =
        claims <- WorldFfi.get userinfo_url (Some ("Bearer " ^ tok)) False;
        claims <- return (fromJson claims : claims);
        name <- return (case claims.Nam of
                            Some name => name
                          | None =>
                            case concato claims.GivenName
                                         (concato claims.MiddleName claims.FamilyName) of
                                None => error <xml>No name in response from OpenID Connect server</xml>
                              | Some name => name);
        case claims.Email of
            None => error <xml>No e-mail address in response from OpenID Connect server</xml>
          | Some email =>
            (if claims.EmailVerified = Some True then
                 return ()
             else
                 error <xml>OpenID Connect server returned an unverified e-mail address.</xml>);
            secret <- oneOrNoRowsE1 (SELECT (secrets.Secret)
                                     FROM secrets
                                     WHERE secrets.Email = {[email]});
            secret <- (case secret of
                           Some secret =>
                           dml (UPDATE secrets
                                SET Nam = {[name]}
                                WHERE Email = {[email]});
                           return secret
                         | None =>
                           secret <- rand;
                           dml (INSERT INTO secrets(Email, Nam, Secret)
                            VALUES ({[email]}, {[name]}, {[secret]}));
                           return secret);

            setCookie user {Value = {Email = email, Secret = secret},
                            Expires = None,
                            Secure = https}

    open Oauth.Make(struct
                        open M

                        val withToken = withToken
                        val scope = None
                    end)

    val whoami =
        c <- getCookie user;
        case c of
            None => return None
          | Some r =>
            ok <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM secrets
                            WHERE secrets.Email = {[r.Email]}
                              AND secrets.Secret = {[r.Secret]});
            if ok then
                return (Some r.Email)
            else
                return None

    val name =
        c <- getCookie user;
        case c of
            None => return None
          | Some r =>
            oneOrNoRowsE1 (SELECT (secrets.Nam)
                           FROM secrets
                           WHERE secrets.Email = {[r.Email]}
                             AND secrets.Secret = {[r.Secret]})

    val logout = clearCookie user
end
