val get : url -> option string (* auth header *) -> transaction string
val post : url -> option string (* auth header *) -> option string (* body Content-type *) -> string (* body *) -> transaction string
val put : url -> option string (* auth header *) -> option string (* body Content-type *) -> string (* body *) -> transaction string
val delete : url -> option string (* auth header *) -> transaction string

type signatur
val length : signatur -> int
val byte : signatur -> int -> char

val sign : string (* key, in PEM format *)
           -> string (* message to sign *)
           -> signatur
