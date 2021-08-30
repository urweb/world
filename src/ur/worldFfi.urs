val get : url -> option string (* auth header *) -> bool (* return normally on errors, with server's error messages? *) -> transaction string
val getOpt : url -> option string (* auth header *) -> bool (* return normally on errors, with server's error messages? *) -> transaction (option string) (* [None] on 404 HTTP code *)
val post : url -> option string (* auth header *) -> option string (* body Content-type *) -> string (* body *) -> transaction string
val put : url -> option string (* auth header *) -> option string (* body Content-type *) -> string (* body *) -> transaction string
val delete : url -> option string (* auth header *) -> transaction string
val patch : url -> option string (* auth header *) -> option string (* body Content-type *) -> string (* body *) -> transaction string

val lastErrorCode : transaction int (* last HTTP code returned by server *)

type signatur
val length : signatur -> int
val byte : signatur -> int -> char

val sign_rs256 : string (* key, in PEM format *)
                 -> string (* message to sign *)
                 -> signatur
val sign_hs256 : string (* key, in PEM format *)
                 -> string (* message to sign *)
                 -> signatur
val scrypt : string (* password *)
             -> string (* salt *)
             -> signatur

val allowHttp : transaction unit
