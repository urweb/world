structure Scope : sig
    type t
    val empty : t
    val union : t -> t -> t
    val readonly : t -> bool
end

signature AUTH = sig
    val instance : string
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
    val instance : string
    val authorize : transaction page
    val status : transaction xbody
    val logout : transaction unit
end

type incident = {
     Description : string
}

type tabl = {
     Nam : string
}

type column = {
     Nam : string,
     Typ : string
}

functor Make(M : AUTH) : sig
    structure Incidents : sig
	val list : transaction (list incident)
    end

    structure Tables : sig
	val list : transaction (list tabl)
	val columns : string -> transaction (list column)
    end
end
