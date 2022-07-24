open Json
open Urls

val sread = mkRead' (fn s =>
                        if s <> "" && Char.isAlpha (String.sub s 0)
                           && String.all (fn ch => Char.isAlpha ch || Char.isDigit ch) s then
                            Some s
                        else
                            None)

type stable = string
val read_stable = sread "NetSuite table"
val show_stable = _

type sfield = string
val read_sfield = sread "NetSuite field"
val show_sfield = _

type settings = {
     AccountId : string,
     ConsumerKey : string,
     ConsumerSecret : string,
     TokenId : string,
     TokenSecret : string
}

type parameters = list (string * string)

fun baseString (ps : parameters) =
    List.foldl (fn (key, value) s =>
                   let
                       val kv = urlencode key ^ "=" ^ urlencode value
                   in
                       case s of
                           "" => kv
                         | _ => s ^ "&" ^ kv
                   end) "" ps

fun headerFor (ps : parameters) =
    List.foldl (fn (key, value) s => s ^ urlencode key ^ "=\"" ^ urlencode value ^ "\", ") "" ps

fun token (stn : settings) =
    tm <- now;
    nonce <- rand;
    ps <- return (("oauth_consumer_key", stn.ConsumerKey)
                      :: ("oauth_nonce", show nonce)
                      :: ("oauth_signature_method", "HMAC-SHA256")
                      :: ("oauth_timestamp", show (toSeconds tm))
                      :: ("oauth_token", stn.TokenId)
                      :: ("oauth_version", "1.0")
                      :: []);
    return (Some (stn.AccountId ^ " "
                  ^ urlencode stn.ConsumerSecret ^ "&" ^ urlencode stn.TokenSecret ^ " "
                  ^ baseString ps ^ " "
                  ^ "OAuth realm=\"" ^ urlencode (String.mp Char.toUpper stn.AccountId) ^ "\", "
                  ^ headerFor ps))

fun finishSignature (key : string) (baseString : string) (method : string) (url : string) =
    let
        val combined = method ^ "&" ^ urlencode url ^ "&" ^ urlencode baseString
        val signed = WorldFfi.sign_hs256 key combined
    in
        "oauth_signature=\"" ^ urlencode (base64_encode_signature signed) ^ "\""

    end

functor TwoLeggedDyn(M : sig
                         val settings : transaction settings
                     end) = struct
    open M

    val token = stn <- settings; token stn
    val logout = return ()
    val status = return <xml></xml>
end

functor TwoLegged(M : sig
                      val account_id : string
                      val consumer_key : string
                      val consumer_secret : string
                      val token_id : string
                      val token_secret : string
                    end) = struct
    open M

    val token = token {AccountId = account_id,
                       ConsumerKey = consumer_key,
                       ConsumerSecret = consumer_secret,
                       TokenId = token_id,
                       TokenSecret = token_secret}
    val logout = return ()
    val status = return <xml></xml>
end

datatype binop =
         Eq
       | NotEq
       | And

datatype expr f rf =
         Field of f
       | RField of rf
       | String of string
       | Null
       | Binop of binop * expr f rf * expr f rf

type exp' (ts :: {Type}) (rts :: {{Type}}) =
     expr (variant (map (fn _ => unit) ts))
          (variant (map (fn ts => variant (map (fn _ => unit) ts)) rts))

type exp (ts :: {Type}) (rts :: {{Type}}) (t :: Type) =
     exp' ts rts

val field [nm :: Name] [t ::: Type] [r ::: {Type}] [rts ::: {{Type}}] [[nm] ~ r] =
    Field (make [nm] ())
val rfield [r ::: {Type}] [nm :: Name] [fnm :: Name] [t ::: Type] [ts ::: {Type}] [rts ::: {{Type}}] [[nm] ~ rts] [[fnm] ~ ts] =
    RField (make [nm] (make [fnm] ()))
fun string [ts ::: {Type}] [rts ::: {{Type}}] (s : string) =
    String s
fun stringOpt [ts ::: {Type}] [rts ::: {{Type}}] (so : option string) =
    case so of
        None => Null
      | Some s => String s
val null [ts ::: {Type}] [rts ::: {{Type}}] =
    Null
fun eq [ts ::: {Type}] [rts ::: {{Type}}] [t ::: Type] (a : exp ts rts t) (b : exp ts rts t) =
    Binop (Eq, a, b)
fun notEq [ts ::: {Type}] [rts ::: {{Type}}] [t ::: Type] (a : exp ts rts t) (b : exp ts rts t) =
    Binop (NotEq, a, b)

type query (full :: {Type}) (rfull :: {{Type}}) (chosen :: {Type}) =
     {Select : string (* this table *) -> $(map (fn _ => string) full) -> $(map (fn ts => string * string * $(map (fn _ => string) ts)) rfull) -> string,
      From : string -> $(map (fn _ => string) full) -> $(map (fn ts => string * string * $(map (fn _ => string) ts)) rfull) -> string,
      Json : $(map (fn _ => string) full) -> $(map (fn ts => string * string (* second string is name of table we link to *) * $(map (fn _ => string) ts)) rfull)
             -> $(map json full) -> $(map (fn ts => $(map json ts)) rfull)
             -> $(map (fn t => string * json t) chosen),
      Where : option (exp' full rfull),
      OrderBy : list (variant (map (fn _ => unit) full) * bool)}

fun select [chosen :: {Type}] [unchosen ::: {Type}] [rts ::: {{Type}}] [chosen ~ unchosen] (fl : folder chosen) =
    {Select = fn this labels _ =>
                 @foldR [fn _ => string] [fn _ => string]
                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (l : string) (acc : string) =>
                      if acc = "" then
                          this ^ "." ^ l
                      else
                          acc ^ "," ^ this ^ "." ^ l)
                  "" fl (labels --- _),
     From = fn _ _ _ => "",
     Json = fn labels _ jsons _ =>
               @map2 [fn _ => string] [json] [fn t => string * json t]
                (fn [t] (l : string) (j : json t) => (String.mp Char.toLower l, j))
                fl (labels --- _) (jsons --- _),
     Where = None,
     OrderBy = []}

fun rselect [ts ::: {Type}] [nm :: Name] [rchosen :: {Type}] [runchosen ::: {Type}]
            [rest ::: {{Type}}] [chosen ::: {Type}]
            [rchosen ~ runchosen] [[nm] ~ rest] [rchosen ~ chosen]
            (fl : folder rchosen)
            (q : query ts ([nm = rchosen ++ runchosen] ++ rest) chosen) =
    q -- #Select -- #From -- #Json
      ++ {Select = fn this labels (rlabels : $(map (fn ts => string * string * $(map (fn _ => string) ts)) ([nm = rchosen ++ runchosen] ++ rest))) =>
                      @foldR [fn _ => string] [fn _ => string]
                       (fn [nm' ::_] [t ::_] [r ::_] [[nm'] ~ r] (l : string) (acc : string) =>
                           if acc = "" then
                               l
                           else
                               acc ^ "," ^ rlabels.nm.1 ^ "." ^ l ^ " AS " ^ rlabels.nm.1 ^ "_" ^ l)
                       (q.Select this labels rlabels) fl (rlabels.nm.3 --- _),
          From = fn this labels (rlabels : $(map (fn ts => string * string * $(map (fn _ => string) ts)) ([nm = rchosen ++ runchosen] ++ rest))) =>
                    q.From this labels rlabels ^ " LEFT JOIN " ^ rlabels.nm.2 ^ " AS " ^ rlabels.nm.1 ^ " ON " ^ this ^ "." ^ rlabels.nm.1 ^ " = " ^ rlabels.nm.1 ^ ".id",
          Json = fn labels (rlabels : $(map (fn ts => string * string * $(map (fn _ => string) ts)) ([nm = rchosen ++ runchosen] ++ rest)))
                           jsons (rjsons : $(map (fn ts => $(map json ts)) ([nm = rchosen ++ runchosen] ++ rest))) =>
                    q.Json labels rlabels jsons rjsons
                           ++ @map2 [fn _ => string] [json] [fn t => string * json t]
                           (fn [t] (l : string) (j : json t) => (String.mp Char.toLower (rlabels.nm.1 ^ "_" ^ l), j))
                           fl (rlabels.nm.3 --- _) (rjsons.nm --- _)}

fun wher [ts ::: {Type}] [rts ::: {{Type}}] [chosen ::: {Type}] (e : exp ts rts bool) (q : query ts rts chosen) =
    q -- #Where ++ {Where = Some (case q.Where of
                                      None => e
                                    | Some e' => Binop (And, e', e))}

fun orderByAsc [nm :: Name] [t ::: Type] [r ::: {Type}] [rts ::: {{Type}}] [chosen ::: {Type}] [[nm] ~ r]
               (q : query ([nm = t] ++ r) rts chosen) =
    q -- #OrderBy ++ {OrderBy = List.append q.OrderBy ((make [nm] (), True) :: [])}

fun orderByDesc [nm :: Name] [t ::: Type] [r ::: {Type}] [rts ::: {{Type}}] [chosen ::: {Type}] [[nm] ~ r]
               (q : query ([nm = t] ++ r) rts chosen) =
    q -- #OrderBy ++ {OrderBy = List.append q.OrderBy ((make [nm] (), False) :: [])}

fun vproj [K] [t ::: Type] [r ::: {K}] (fl : folder r) (r : $(map (fn _ => t) r)) (v : variant (map (fn _ => unit) r)) =
    match v (@Top.mp [fn _ => t] [fn _ => unit -> t]
             (fn [u] v () => v)
             fl r)

fun vproj' [K] [tf ::: K -> Type] [tf' :: K -> Type] [t ::: Type] [r ::: {K}] (fl : folder r) (r : $(map (fn ts => t * t * tf ts) r)) (v : variant (map tf' r)) =
    match v (@Top.mp [fn ts => t * t * tf ts] [fn ts => tf' ts -> t]
             (fn [u] (v, _, _) _ => v)
             fl r)

fun vproj2 [t1 ::: Type] [t ::: Type] [r ::: {{Type}}] (fl : folder r) (fls : $(map folder r))
           (r : $(map (fn ts => t1 * t1 * $(map (fn _ => t) ts)) r))
           (v : variant (map (fn ts => variant (map (fn _ => unit) ts)) r)) =
    match v (@Top.map2 [folder] [fn ts => t1 * t1 * $(map (fn _ => t) ts)] [fn ts => variant (map (fn _ => unit) ts) -> t]
              (fn [ts] fl' (_, _, r) v => @vproj fl' r v)
              fl fls r)

type values (ts :: {Type}) = {
     Basic : $(map (fn _ => string) ts) -> $(map json ts) -> string,
     Composite : [[Attributes] ~ ts] => $(map (fn _ => string) ts) -> $(map json ts) -> string (* type *) -> string
}

fun values [chosen ::: {Type}] [unchosen ::: {Type}] [chosen ~ unchosen]
           (fl : folder chosen) (r : $chosen) = {
    Basic = fn (labels : $(map (fn _ => string) (chosen ++ unchosen)))
                   (jsons : $(map json (chosen ++ unchosen))) =>
               @toJson (@json_record fl (jsons --- _) (labels --- _)) r,
    Composite = fn [[Attributes] ~ (chosen ++ unchosen)]
                       (labels : $(map (fn _ => string) (chosen ++ unchosen)))
                       (jsons : $(map json (chosen ++ unchosen))) ty =>
                   @toJson (@json_record (@Folder.cons [#Attributes] [_] ! fl)
                             (jsons --- _ ++ {Attributes = json_record {Typ = "type"}})
                             (labels --- _ ++ {Attributes = "attributes"}))
                    (r ++ {Attributes = {Typ = ty}})
}

type query_request = {
     Q : string
}
val _ : json query_request = json_record {Q = "q"}

type query_results (r :: Type) = {
     Items : list r,
     Count : int,
     Offset : int,
     TotalResults : int
}
fun json_query_results [r] (_ : json r) : json (query_results r) =
    json_record {Items = "items",
                 Count = "count",
                 Offset = "offset",
                 TotalResults = "totalResults"}

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into NetSuite to use this feature.</xml>
          | Some tok =>
            case String.split tok #" " of
                None => error <xml>Malformed NetSuite token</xml>
              | Some (acct, rest) =>
                case String.split rest #" " of
                    None => error <xml>Malformed NetSuite token</xml>
                  | Some (key, rest) =>
                    case String.split rest #" " of
                        None => error <xml>Malformed NetSuite token</xml>
                      | Some (base, rest) => return (acct, key, base, rest)

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("NetSuite response: " ^ show v);
        return v

    fun url acct path =
        bless ("https://" ^ acct ^ ".suitetalk.api.netsuite.com/services/rest/" ^ path)

    fun api path =
        (acct, key, base, auth) <- token;
        url <- return (url acct path);
        auth <- return (auth ^ finishSignature key base "GET" (show url));
        debug ("NetSuite GET: " ^ show url);
        logged (WorldFfi.get url
                             (WorldFfi.addHeader
                                  (WorldFfi.addHeader WorldFfi.emptyHeaders "Prefer" "transient")
                                  "Authorization" auth) False)

    fun apiPost path body =
        (acct, key, base, auth) <- token;
        url <- return (url acct path);
        auth <- return (auth ^ finishSignature key base "POST" (show url));
        debug ("NetSuite POST: " ^ show url);
        debug ("Body: " ^ show body);
        logged (WorldFfi.post url
                             (WorldFfi.addHeader
                                  (WorldFfi.addHeader WorldFfi.emptyHeaders "Prefer" "transient")
                                  "Authorization" auth)
                             (Some "application/json") body)

    fun apiPatch path body =
        (acct, key, base, auth) <- token;
        url <- return (url acct path);
        auth <- return (auth ^ finishSignature key base "PATCH" (show url));
        debug ("NetSuite PATCH: " ^ show url);
        debug ("Body: " ^ show body);
        logged (WorldFfi.patch url
                               (WorldFfi.addHeader
                                    (WorldFfi.addHeader WorldFfi.emptyHeaders "Prefer" "transient")
                                    "Authorization" auth)
                               (Some "application/json") body)

    functor Table(N : sig
                      val stable : stable
                      con fields :: {Type}
                      constraint [Id] ~ fields
                      val labels : $(map (fn _ => string) fields)
                      val jsons : $(map json fields)
                      val fl : folder fields

                      con relations :: {{Type}}
                      val rlabels : $(map (fn ts => string * string * $(map (fn _ => string) ts)) relations)
                      val rjsons : $(map (fn ts => $(map Json.json ts)) relations)
                      val rfl : folder relations
                      val rfls : $(map folder relations)
                  end) = struct
        open N
        con fields' = [Id = string] ++ fields

        fun escapeSingleQuotes s =
            case s of
                "" => ""
              | _ =>
                let
                    val ch = String.sub s 0
                in
		    (case ch of
		         #"'" => "\\'"
		       | #"\\" => "\\\\"
		       | x => String.str ch)
                    ^ escapeSingleQuotes (String.suffix s 1)
                end

        fun formatBinop b =
            case b of
                Eq => "="
              | NotEq => "!="
              | And => "AND"

        fun allowsParens b =
            case b of
                Eq => False
              | NotEq => False
              | And => True

        val labels = {Id = "Id"} ++ labels
        val jsons = {Id = _} ++ jsons
        val fl' : folder fields' = @Folder.cons [#Id] [_] ! fl
        val labelOf = @vproj fl' labels
        fun rlabelOf (rf : variant (map (fn ts => variant (map (fn _ => unit) ts)) relations)) =
            @@vproj' [fn ts => $(map (fn _ => string) ts)] [fn ts => variant (map (fn _ => unit) ts)]
              [string] [_]
              rfl rlabels rf ^ "." ^ @vproj2 rfl rfls rlabels rf

        fun formatExp (e : exp' fields' relations) =
            case e of
                String s => "'" ^ escapeSingleQuotes s ^ "'"
              | Null => "NULL"
              | Field f => labelOf f
              | RField rf => rlabelOf rf
              | Binop (b, e1, e2) =>
                if allowsParens b then
                    "(" ^ formatExp e1 ^ ")"
                    ^ formatBinop b
                    ^ "(" ^ formatExp e2 ^ ")"
                else
                    formatExp e1 ^ " "
                    ^ formatBinop b
                    ^ " " ^ formatExp e2

        fun formatOrderBy1 (v, b) =
            labelOf v ^ " " ^ (if b then "ASC" else "DESC")

        fun formatQuery [chosen] (q : query fields' relations chosen) =
            let
                val qu = "SELECT " ^ (q.Select : string -> $(map (fn _ => string) fields') -> $(map (fn ts => string * string * $(map (fn _ => string) ts)) relations) -> string) stable labels rlabels ^ " FROM " ^ stable ^ q.From stable labels rlabels

                val qu = case q.Where of
                             None => qu
                           | Some e => qu ^ " WHERE " ^ formatExp e

                val qu = case q.OrderBy of
                             [] => qu
                           | ob1 :: obs =>
                             List.foldl (fn ob acc =>
                                            acc ^ "," ^ formatOrderBy1 ob)
                             (qu ^ " ORDER BY " ^ formatOrderBy1 ob1) obs
            in
                qu
            end

        fun query [chosen] (fl : folder chosen) (q : query fields' relations chosen) =
            let
                fun retrieve offset acc =
                    s <- apiPost ("query/v1/suiteql"
                                  ^ case offset of
                                        None => ""
                                      | Some offset => "?offset=" ^ show offset)
                                 (toJson {Q = @formatQuery q});
                    r <- return (@fromJson (@json_query_results (@json_record_withOptional ! _ {} {}
                                                                  fl
                                                                  (@mp [fn t => string * json t] [json]
                                                                       (fn [t] (p : string * json t) => p.2)
                                                                       fl (q.Json labels rlabels jsons rjsons))
                                                                  (@mp [fn t => string * json t] [fn _ => string]
                                                                       (fn [t] (p : string * json t) => p.1)
                                                                       fl (q.Json labels rlabels jsons rjsons)))) s : query_results $(map option chosen));

                    if r.Offset + r.Count < r.TotalResults then
                        retrieve (Some (r.Offset + r.Count)) (List.revAppend r.Items acc)
                    else
                        case acc of
                            [] => return r.Items
                          | _ => return (List.rev (List.revAppend r.Items acc))
            in
                retrieve None []
            end

        fun insert vs =
            Monad.ignore (apiPost ("record/v1/" ^ stable) (vs.Basic labels jsons))

        fun update id vs =
            Monad.ignore (apiPatch ("record/v1/" ^ stable ^ "/" ^ Urls.urlencode id) (vs.Basic labels jsons))
    end
end
