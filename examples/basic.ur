structure O = Oauth.Make(struct
                             open Basic_in

                             val authorize_url = bless "https://github.com/login/oauth/authorize"
                             val access_token_url = bless "https://github.com/login/oauth/access_token"
                         end)

val after =
    return <xml>Welcome back.</xml>

fun main () =
    O.authorize {ReturnTo = url after}
