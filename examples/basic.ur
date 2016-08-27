structure G = Github.Make(struct
                             open Basic_in
                             val https = False
                         end)

val after =
    u <- G.whoami;
    return <xml>Welcome back, {[u]}.</xml>

fun main () =
    G.authorize {ReturnTo = url after}
