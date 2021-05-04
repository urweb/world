signature S = sig
    val authorize_url : url
    val access_token_url : url
    val userinfo_url : url
    val client_id : string
    val client_secret : string
    val https : bool
    val onCompletion : transaction page
    val scopeValue : option string (* if present, pass to authorization endpoint *)
end

functor Make(M : S) : sig
    val authorize : transaction page
    val email : transaction (option string)
    val name : transaction (option string)
    val logout : transaction unit
end
