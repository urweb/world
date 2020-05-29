type instance
val read_instance : read instance
val show_instance : show instance

functor ThreeLegged(M : sig
                        val client_id : string
                        val client_secret : string

                        val https : bool
                        val sandbox : bool

                        val onCompletion : transaction page
                    end) : sig
    val token : transaction (option string)
    val status : transaction xbody
end

type stable (* a kind of SObject, e.g. Account, Contact *)
val read_stable : read stable
val show_stable : show stable

type sfield
val read_sfield : read sfield
val show_sfield : show sfield

con exp :: {Type} (* direct fields *) -> {{Type}} (* fields via relations *) -> Type -> Type
val field : nm :: Name -> t ::: Type -> r ::: {Type} -> rts ::: {{Type}} -> [[nm] ~ r]
            => exp ([nm = t] ++ r) rts t
val string : ts ::: {Type} -> rts ::: {{Type}} -> string -> exp ts rts string
val eq : ts ::: {Type} -> rts ::: {{Type}} -> t ::: Type -> exp ts rts t -> exp ts rts t -> exp ts rts bool

con query :: {Type} -> {{Type}} -> {Type} -> Type
val select : chosen :: {Type} -> unchosen ::: {Type} -> rts ::: {{Type}} -> [chosen ~ unchosen]
    => folder chosen
    -> query (chosen ++ unchosen) rts chosen
val rselect : ts ::: {Type} -> nm :: Name -> rchosen :: {Type} -> runchosen ::: {Type} -> rest ::: {{Type}} -> chosen ::: {Type}
              -> [rchosen ~ runchosen] => [[nm] ~ rest] => [[nm] ~ chosen]
              => folder rchosen
              -> query ts ([nm = rchosen ++ runchosen] ++ rest) chosen
              -> query ts ([nm = rchosen ++ runchosen] ++ rest) ([nm = $rchosen] ++ chosen)
val wher : ts ::: {Type} -> rts ::: {{Type}} -> chosen ::: {Type}
           -> exp ts rts bool -> query ts rts chosen -> query ts rts chosen
val orderByAsc : nm :: Name -> t ::: Type -> r ::: {Type} -> rts ::: {{Type}} -> chosen ::: {Type} -> [[nm] ~ r]
                 => query ([nm = t] ++ r) rts chosen -> query ([nm = t] ++ r) rts chosen
val orderByDesc : nm :: Name -> t ::: Type -> r ::: {Type} -> rts ::: {{Type}} -> chosen ::: {Type} -> [[nm] ~ r]
                  => query ([nm = t] ++ r) rts chosen -> query ([nm = t] ++ r) rts chosen

con values :: {Type} -> Type
val values : chosen ::: {Type} -> unchosen ::: {Type} -> [chosen ~ unchosen]
             => folder chosen -> $chosen -> values (chosen ++ unchosen)

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    val record : instance -> string (* object ID *) -> url
    (* The canonical page to examine a record *)

    functor Table(N : sig
                      val stable : stable
                      con fields :: {Type}
                      val labels : $(map (fn _ => string) fields)
                      val jsons : $(map Json.json fields)
                      val fl : folder fields

                      con relations :: {{Type}}
                      val rlabels : $(map (fn ts => string * $(map (fn _ => string) ts)) relations)
                      val rjsons : $(map (fn ts => $(map Json.json ts)) relations)
                      val rfl : folder relations
                      val rfls : $(map folder relations)
                  end) : sig
        val query : chosen ::: {Type} -> folder chosen -> instance -> query N.fields N.relations chosen -> transaction (list $chosen)
        val insert : instance -> values N.fields -> transaction string
    end
end
