signature S = sig
    val authorize_url : url
    val access_token_url : url

    val client_id : string
    val client_secret : string
    val scope : option string

    val withToken : {Token : string, Expiration : option int (* seconds from now *)} -> transaction unit
    val onCompletion : transaction page (* run this after we're logged in, e.g. to return to the page where the user asked to log in *)
end

functor Make(M : S) : sig
    val authorize : transaction page
end
