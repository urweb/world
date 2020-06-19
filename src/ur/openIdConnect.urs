signature S = sig
    val authorize_url : url
    val access_token_url : url
    val userinfo_url : url
    val client_id : string
    val client_secret : string
    val https : bool
    val onCompletion : transaction page
end

functor Make(M : S) : sig
    val authorize : transaction page
    val whoami : transaction (option string)
    val name : transaction (option string)
    val logout : transaction unit
end
