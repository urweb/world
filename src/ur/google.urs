signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

functor Make(M : S) : sig
    val authorize : { ReturnTo : url } -> transaction page
    val whoami : transaction (option string)
    val logout : transaction unit
end
