(* Code for handling service-specific notions of scopes, a la OAuth 2.
 * In other words, we manage subsets of a set of named permissions. *)

con t :: {Unit} (* full set of permissions *) -> Type

val empty : ps ::: {Unit} -> folder ps -> t ps
val one : p :: Name -> ps ::: {Unit} -> [[p] ~ ps] => folder ps -> t ([p] ++ ps)
val union : ps ::: {Unit} -> folder ps -> t ps -> t ps -> t ps
val disjoint : ps ::: {Unit} -> folder ps -> t ps -> t ps -> bool
val toString : ps ::: {Unit} -> folder ps -> $(mapU string ps) -> t ps -> string
