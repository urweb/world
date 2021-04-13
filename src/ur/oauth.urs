signature S = sig
    val authorize_url : url
    val access_token_url : url

    val client_id : string
    val client_secret : string
    val scope : option string
    val hosted_domain : option string (* a hint on which e-mail addresses to accept,
                                       * via giving one domain *)

    val withToken : {Token : string, Expiration : option int (* seconds from now *)} -> transaction unit
    val onCompletion : transaction page (* run this after we're logged in, e.g. to return to the page where the user asked to log in *)
    val nameForScopeParameter : option string (* some providers use a different query-parameter name in authorization URLs *)
    val parseTokenResponse : option (string -> {Token : string, Expires : option int}) (* some providers use weird formats for the 2nd step *)
end

functor Make(M : S) : sig
    val authorize : transaction page
end
