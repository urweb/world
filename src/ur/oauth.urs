signature S = sig
    val scope : option string
    val hosted_domain : option string (* a hint on which e-mail addresses to accept,
                                       * via giving one domain *)

    val withToken : {Token : string, Expiration : option int (* seconds from now *)} -> transaction unit
    val onCompletion : transaction page (* run this after we're logged in, e.g. to return to the page where the user asked to log in *)
    val nameForScopeParameter : option string (* some providers use a different query-parameter name in authorization URLs *)
    val parseTokenResponse : option (string -> {Token : string, Expires : option int}) (* some providers use weird formats for the 2nd step *)
end

functor Make(M : sig
                 include S

                 val authorize_url : url
                 val access_token_url : url

                 val client_id : string
                 val client_secret : string
             end) : sig
    val authorize : transaction page
end

type settings = {
     AuthorizeUrl : url,
     AccessTokenUrl : url,
     ClientId : string,
     ClientSecret : string
}

(* This version allows some of the parameters to be determined at runtime. *)
functor MakeDyn(M : sig
                    include S

                    val settings : transaction settings
                end) : sig
    val authorize : transaction page
end
