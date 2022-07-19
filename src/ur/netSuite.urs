functor TwoLegged(M : sig
                      val account_id : string
                      val consumer_key : string
                      val consumer_secret : string
                      val token_id : string
                      val token_secret : string
                    end) : sig
    val token : transaction (option string)
    val status : transaction xbody
    val logout : transaction unit
end

type settings = {
     AccountId : string,
     ConsumerKey : string,
     ConsumerSecret : string,
     TokenId : string,
     TokenSecret : string
}

(* Allow some details to be determined at runtime. *)
functor TwoLeggedDyn(M : sig
                         val settings : transaction settings
                     end) : sig
    val token : transaction (option string)
    val status : transaction xbody
    val logout : transaction unit
end

type stable (* database table *)
val read_stable : read stable
val show_stable : show stable

type sfield
val read_sfield : read sfield
val show_sfield : show sfield

con exp :: {Type} (* direct fields *) -> {{Type}} (* fields via relations *) -> Type -> Type
val field : nm :: Name -> t ::: Type -> r ::: {Type} -> rts ::: {{Type}} -> [[nm] ~ r]
            => exp ([nm = t] ++ r) rts t
val rfield : r ::: {Type} -> nm :: Name -> fnm :: Name -> t ::: Type -> ts ::: {Type} -> rts ::: {{Type}} -> [[nm] ~ rts] => [[fnm] ~ ts]
             => exp r ([nm = [fnm = t] ++ ts] ++ rts) t
val string : ts ::: {Type} -> rts ::: {{Type}} -> string -> exp ts rts string
val stringOpt : ts ::: {Type} -> rts ::: {{Type}} -> option string -> exp ts rts (option string)
val null : ts ::: {Type} -> rts ::: {{Type}} -> exp ts rts string
val eq : ts ::: {Type} -> rts ::: {{Type}} -> t ::: Type -> exp ts rts t -> exp ts rts t -> exp ts rts bool
val notEq : ts ::: {Type} -> rts ::: {{Type}} -> t ::: Type -> exp ts rts t -> exp ts rts t -> exp ts rts bool

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
    structure Metadata : sig
        val tables : transaction (list stable)
        val schema : stable -> transaction OpenAPI.Schema.r
    end
end
