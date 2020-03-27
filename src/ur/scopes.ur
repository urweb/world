type t (ps :: {Unit}) = $(mapU bool ps)

fun empty [ps ::: {Unit}] (fl : folder ps) =
    @map0 [fn _ => bool]
     (fn [t ::_] => False)
     fl

fun one [p :: Name] [ps ::: {Unit}] [[p] ~ ps] (fl : folder ps) =
    {p = True} ++ @empty fl
     
fun union [ps ::: {Unit}] (fl : folder ps) (a : t ps) (b : t ps) =
    @map2 [fn _ => bool] [fn _ => bool] [fn _ => bool]
     (fn [u] b1 b2 => b1 || b2)
     fl a b

fun disjoint [ps ::: {Unit}] (fl : folder ps) (a : t ps) (b : t ps) =
    @foldUR2 [bool] [bool] [fn _ => bool]
     (fn [nm ::_] [r ::_] [[nm] ~ r] b1 b2 acc => acc && not (b1 && b2))
     True fl a b
    
fun toString [ps ::: {Unit}] (fl : folder ps) (labels : $(mapU string ps)) (t : t ps) =
    @foldUR2 [string] [bool] [fn _ => string]
    (fn [nm ::_] [r ::_] [[nm] ~ r] l b acc =>
        if b then
            case acc of
                "" => l
              | _ => l ^ " " ^ acc
        else
            acc)
    "" fl labels t
