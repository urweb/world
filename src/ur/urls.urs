val urlencode : string -> string
val urldecode : string -> string

val base64url_encode : string -> string
val base64url_encode_signature : WorldFfi.signatur -> string
val base64_encode_signature : WorldFfi.signatur -> string

(* Takes a base URL along with a record of name/value pairs and formats the
   pairs into the typical "base_url?nm1=val1&nm2=val2...", properly URL encoding
   the given strings.
   The base URL may already name/value pairs.
   If the value is `None`, then the name/value pair is omitted from the result *)
val add_params_to_url : r ::: {Unit} -> folder r -> url -> $(mapU (string * option string) r) -> url
