open Json

structure Scope = struct
    type t = Scopes.t []
    val empty = Scopes.empty
    val union = Scopes.union
    val toString = Scopes.toString {}

    val readonly = Scopes.disjoint empty
end

signature AUTH = sig
    val token : transaction (option string)
end

functor Make(M : AUTH) = struct
    open M
end

functor ThreeLegged(M : sig
			val instance : string
                        val client_id : string
                        val client_secret : string
                        val https : bool

                        val scopes : Scope.t
                        val onCompletion : transaction page
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
        case seconds of
            None => error <xml>Missing token expiration in OAuth response</xml>
          | Some seconds =>
            secret <- rand;
            tm <- now;
            dml (INSERT INTO secrets(Secret, Token, Expires)
                 VALUES ({[secret]}, {[tok]}, {[addSeconds tm (seconds * 3 / 4)]}));
            setCookie user {Value = secret,
                            Expires = None,
                            Secure = https}

    open Oauth.Make(struct
                        open M

                        val authorize_url = bless ("https://" ^ instance ^ ".service-now.com/oauth_auth.do")
                        val access_token_url = bless ("https://" ^ instance ^ ".service-now.com/oauth_token.do")

                        val withToken = withToken
                        val scope = None
                        val nameForScopeParameter = None
                        val parseTokenResponse = None
                        val hosted_domain = None
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

    val logout = clearCookie user

    val status =
        toko <- token;
        li <- source (Option.isSome toko);
        cur <- currentUrl;
        return <xml>
          <dyn signal={liV <- signal li;
                       if liV then
                           return <xml><button value="Log out of ServiceNow"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into ServiceNow"
                                               onclick={fn _ => redirect (url authorize)}/></xml>}/>
        </xml>
end
