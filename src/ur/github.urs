table users : { Login : string,
                AvatarUrl : string,
                Nam : option string,
                Company : option string,
                Blog : option string,
                Location : option string,
                Email : option string,
                Hireable : option bool,
                Bio : option string,
                LastUpdated : time }
  PRIMARY KEY Login

signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

functor Make(M : S) : sig
    val authorize : { ReturnTo : url } -> transaction page
    val whoami : transaction (option string)
    val trackUser : string -> transaction unit
end
