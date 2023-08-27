structure Scope : sig
    type t
    val empty : t
    val union : t -> t -> t
    val readonly : t -> bool
end

signature AUTH = sig
    val token : transaction (option string)
end

functor ThreeLegged(M : sig
			val instance : string
                        val client_id : string
                        val client_secret : string
                        val https : bool

                        val scopes : Scope.t
                        val onCompletion : transaction page
                    end) : sig
    val token : transaction (option string)
    val authorize : transaction page
    val status : transaction xbody
    val logout : transaction unit
end

functor Make(M : AUTH) : sig
end
