structure Scope : sig
    type t
    val empty : t
    val union : t -> t -> t
    val readonly : t -> bool
end

signature AUTH = sig
    val instance : transaction string
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
    val instance : transaction string
    val authorize : transaction page
    val status : transaction xbody
    val logout : transaction unit
end

type settings = {
     ClientId : string,
     ClientSecret : string,
     Instance : string
}

functor ThreeLeggedDyn(M : sig
			   val settings : transaction settings

                           val https : bool

                           val scopes : Scope.t
                           val onCompletion : transaction page
                    end) : sig
    val token : transaction (option string)
    val instance : transaction string
    val authorize : transaction page
    val status : transaction xbody
    val logout : transaction unit
end

type incident = {
     Description : string
}

type table_name = {
     Nam : string
}

type column = {
     Nam : string,
     Typ : string
}

type full_table = {
     Columns : list column,
     DisplayColumn : option string
}

functor Make(M : AUTH) : sig
    structure Incidents : sig
        val list : transaction (list incident)
    end

    structure Tables : sig
        val list : transaction (list table_name)
        val columns : string -> transaction full_table
    end

    structure Table : sig
        val list : ts ::: {Type}
                   -> folder ts
                   -> $(map (fn _ => string) ts) (* labels in JSON *)
                   -> $(map Json.json ts)
                   -> string (* table name *)
                   -> transaction (list $(map option ts))
    end
end
