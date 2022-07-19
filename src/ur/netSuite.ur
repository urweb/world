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
     {Select : $(map (fn _ => string) full) -> $(map (fn ts => string * $(map (fn _ => string) ts)) rfull) -> string,
      Json : $(map (fn _ => string) full) -> $(map (fn ts => string * $(map (fn _ => string) ts)) rfull)
             -> $(map json full) -> $(map (fn ts => $(map json ts)) rfull)
             -> $(map (fn t => string * json t) chosen),
      Where : option (exp' full rfull),
      OrderBy : list (variant (map (fn _ => unit) full) * bool)}

fun select [chosen :: {Type}] [unchosen ::: {Type}] [rts ::: {{Type}}] [chosen ~ unchosen] (fl : folder chosen) =
    {Select = fn labels _ =>
                 @foldR [fn _ => string] [fn _ => string]
                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (l : string) (acc : string) =>
                      if acc = "" then
                          l
                      else
                          acc ^ "," ^ l)
                  "" fl (labels --- _),
     Json = fn labels _ jsons _ =>
               @map2 [fn _ => string] [json] [fn t => string * json t]
                (fn [t] (l : string) (j : json t) => (l, j))
                fl (labels --- _) (jsons --- _),
     Where = None,
     OrderBy = []}

fun rselect [ts ::: {Type}] [nm :: Name] [rchosen :: {Type}] [runchosen ::: {Type}]
            [rest ::: {{Type}}] [chosen ::: {Type}]
            [rchosen ~ runchosen] [[nm] ~ rest] [[nm] ~ chosen]
            (fl : folder rchosen)
            (q : query ts ([nm = rchosen ++ runchosen] ++ rest) chosen) =
    q -- #Select -- #Json
      ++ {Select = fn labels (rlabels : $(map (fn ts => string * $(map (fn _ => string) ts)) ([nm = rchosen ++ runchosen] ++ rest))) =>
                      @foldR [fn _ => string] [fn _ => string]
                       (fn [nm' ::_] [t ::_] [r ::_] [[nm'] ~ r] (l : string) (acc : string) =>
                           if acc = "" then
                               l
                           else
                               acc ^ "," ^ rlabels.nm.1 ^ "." ^ l)
                       (q.Select labels rlabels) fl (rlabels.nm.2 --- _),
          Json = fn labels (rlabels : $(map (fn ts => string * $(map (fn _ => string) ts)) ([nm = rchosen ++ runchosen] ++ rest)))
                           jsons (rjsons : $(map (fn ts => $(map json ts)) ([nm = rchosen ++ runchosen] ++ rest))) =>
                    q.Json labels rlabels jsons rjsons
                           ++ {nm = (rlabels.nm.1,
                                     @json_record fl (rjsons.nm --- _) (rlabels.nm.2 --- _))}}

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

fun vproj' [K] [tf ::: K -> Type] [tf' :: K -> Type] [t ::: Type] [r ::: {K}] (fl : folder r) (r : $(map (fn ts => t * tf ts) r)) (v : variant (map tf' r)) =
    match v (@Top.mp [fn ts => t * tf ts] [fn ts => tf' ts -> t]
             (fn [u] (v, _) _ => v)
             fl r)

fun vproj2 [t1 ::: Type] [t ::: Type] [r ::: {{Type}}] (fl : folder r) (fls : $(map folder r))
           (r : $(map (fn ts => t1 * $(map (fn _ => t) ts)) r))
           (v : variant (map (fn ts => variant (map (fn _ => unit) ts)) r)) =
    match v (@Top.map2 [folder] [fn ts => t1 * $(map (fn _ => t) ts)] [fn ts => variant (map (fn _ => unit) ts) -> t]
              (fn [ts] fl' (_, r) v => @vproj fl' r v)
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

type catalog_item = {Nam : string}
val _ : json catalog_item = json_record {Nam = "name"}

type catalog = {Items : list catalog_item}
val _ : json catalog = json_record {Items = "items"}

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

    fun apiWithAccept accept path =
        (acct, key, base, auth) <- token;
        url <- return (url acct path);
        auth <- return (auth ^ finishSignature key base "GET" (show url));
        debug ("NetSuite GET: " ^ show url);
        logged (WorldFfi.get url
                             (WorldFfi.addHeader
                                  (WorldFfi.addHeader
                                       (WorldFfi.addHeader WorldFfi.emptyHeaders
                                                           "Accept" accept)
                                       "Prefer" "transient")
                                  "Authorization" auth) False)

    structure Metadata = struct
        val tables =
            s <- api "record/v1/metadata-catalog";
            return (List.mp (fn r => r.Nam) (fromJson s : catalog).Items)

        fun schema tname =
            s <- apiWithAccept "application/schema+json" ("record/v1/metadata-catalog/" ^ tname);
            return (fromJson s)
    end
end
