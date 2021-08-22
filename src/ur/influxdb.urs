functor TwoLegged(M : sig
                      val influxdb_url : string (* Base URL you use to access this database on the web *)
                      val org : string          (* Organization name *)
                      val bucket : string       (* Bucket name *)
                      val api_token : string
                    end) : sig
    val token : transaction (option string)
end

type line = {Measurement : string,
             Tags : list (string * string),
             Fields : list (string * string),
             Timestamp : option int}
type lines = list line
(* Lists of string pairs are key-value associations.
 * Keys (including measurement names) should only be alphanumeric, perhaps with "-" and "_". *)

(* Unit of measure for timestamps *)
datatype precision = S | Ms | Us | Ns

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    val write : {Precision : option precision} -> lines -> transaction unit
end
