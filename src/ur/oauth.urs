signature S = sig
    val authorize_url : url
    val access_token_url : url

    val client_id : string
    val client_secret : string
    val scope : option string

    val withToken : {Token : string, Expiration : option int (* seconds from now *)} -> transaction unit
end

functor Make(M : S) : sig
    val authorize : { ReturnTo : url } -> transaction page
end
