open Json

table secrets : { ResourceName : string,
                  Secret : int,
                  Token : string }
  PRIMARY KEY ResourceName

cookie user : { ResourceName : string, Secret : int }

signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

type profile = { ResourceName : string }

val json_profile : json profile =
    json_record {ResourceName = "resourceName"}
              
functor Make(M : S) = struct
    open M

    fun withToken tok =
        profile <- WorldFfi.get (bless "https://people.googleapis.com/v1/people/me?personFields=emailAddresses") (Some ("Bearer " ^ tok));
        (profile : profile) <- return (Json.fromJson profile);
        secret <- oneOrNoRowsE1 (SELECT (secrets.Secret)
                                 FROM secrets
                                 WHERE secrets.ResourceName = {[profile.ResourceName]});
        secret <- (case secret of
                       Some secret => return secret
                     | None =>
                       secret <- rand;
                       dml (INSERT INTO secrets(ResourceName, Secret, Token)
                            VALUES ({[profile.ResourceName]}, {[secret]}, {[tok]}));
                       return secret);

        setCookie user {Value = {ResourceName = profile.ResourceName, Secret = secret},
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M

                        val authorize_url = bless "https://accounts.google.com/o/oauth2/auth"
                        val access_token_url = bless "https://oauth2.googleapis.com/token"
                        val withToken = withToken
                        val scope = Some "profile"
                    end)

    val whoami =
        c <- getCookie user;
        case c of
            None => return None
          | Some r =>
            ok <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM secrets
                            WHERE secrets.ResourceName = {[r.ResourceName]}
                              AND secrets.Secret = {[r.Secret]});
            if ok then
                return (Some r.ResourceName)
            else
                return None

    val logout = clearCookie user
end
