val get : url -> option string (* auth header *) -> transaction string
val post : url -> option string (* auth header *) -> option string (* body Content-type *) -> string (* body *) -> transaction string
val put : url -> option string (* auth header *) -> option string (* body Content-type *) -> string (* body *) -> transaction string
val delete : url -> option string (* auth header *) -> transaction string
