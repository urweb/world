open Json
open Urls

type instance = string
val read_instance = _
val show_instance = _

val sread = mkRead' (fn s =>
                        if s <> "" && Char.isAlpha (String.sub s 0)
                           && String.all (fn ch => Char.isAlpha ch || Char.isDigit ch) s then
                            Some s
                        else
                            None)

type stable = string
val read_stable = sread "Salesforce SObject table"
val show_stable = _

type sfield = string
val read_sfield = sread "Salesforce SObject field"
val show_sfield = _

functor ThreeLegged(M : sig
                        val https : bool
                        val sandbox : bool

                        val client_id : string
                        val client_secret : string

                        val onCompletion : transaction page
                    end) = struct
    open M

    table secrets : { Secret : int,
                      Token : string,
                      Expires : time }
      PRIMARY KEY Secret

    task periodic 60 = fn () =>
                          tm <- now;
                          dml (DELETE FROM secrets
                               WHERE Expires < {[addSeconds tm (-60)]})

    cookie user : int

    fun withToken {Token = tok, Expiration = seconds, ...} =
        seconds <- return (Option.get (30 * 60) seconds);
        secret <- rand;
        tm <- now;
        dml (INSERT INTO secrets(Secret, Token, Expires)
             VALUES ({[secret]}, {[tok]}, {[addSeconds tm (seconds * 3 / 4)]}));
        setCookie user {Value = secret,
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M

                        val url_base = "https://" ^ (if sandbox then "test." else "login.") ^ "salesforce.com/services/oauth2/"
                        val authorize_url = bless (url_base ^ "authorize")
                        val access_token_url = bless (url_base ^ "token")
                        val scope = None

                        val withToken = withToken
                        val nameForScopeParameter = None
                        val parseTokenResponse = None
                    end)

    val token =
        c <- getCookie user;
        case c of
            None => return None
          | Some n =>
            oneOrNoRowsE1 (SELECT (secrets.Token)
                           FROM secrets
                           WHERE secrets.Secret = {[n]}
                             AND secrets.Expires > CURRENT_TIMESTAMP)

    val loggedIn = c <- getCookie user;
        case c of
            None => return False
          | Some s =>
            expiresO <- oneOrNoRowsE1 (SELECT (secrets.Expires)
                                       FROM secrets
                                       WHERE secrets.Secret = {[s]});
            case expiresO of
                None => return False
              | Some exp =>
                tm <- now;
                return (tm < exp)
    val logout = clearCookie user

    val status =
        li <- loggedIn;
        li <- source li;
        cur <- currentUrl;
        return <xml>
          <dyn signal={liV <- signal li;
                       if liV then
                           return <xml><button value="Log out of Salesforce"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into Salesforce"
                                               onclick={fn _ => redirect (url authorize)}/></xml>}/>
        </xml>
end

type query_results (r :: Type) = {
     Records : list r
}
fun json_query_results [r] (_ : json r) : json (query_results r) = json_record {Records = "records"}

type success_response = {
     Id : string
}
val _ : json success_response = json_record {Id = "id"}

type error_response = {
     Message : string
}
val _ : json error_response = json_record {Message = "message"}

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

type values (ts :: {Type}) =
     $(map (fn _ => string) ts) -> $(map json ts) -> string

fun values [chosen ::: {Type}] [unchosen ::: {Type}] [chosen ~ unchosen]
           (fl : folder chosen) (r : $chosen)
           (labels : $(map (fn _ => string) (chosen ++ unchosen)))
           (jsons : $(map json (chosen ++ unchosen))) =
  @toJson (@json_record fl (jsons --- _) (labels --- _)) r

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Salesforce to use this feature.</xml>
          | Some tok => return tok

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("Salesforce response: " ^ show v);
        return v

    fun api url =
        tok <- token;
        logged (WorldFfi.get url (Some ("Bearer " ^ tok)) False)

    fun apiPost url body =
        tok <- token;
        logged (WorldFfi.post url (Some ("Bearer " ^ tok)) (Some "application/json") body)

    fun apiPatch url body =
        tok <- token;
        logged (WorldFfi.patch url (Some ("Bearer " ^ tok)) (Some "application/json") body)

    fun idFromUrl url =
        let
            fun findLastSlash s suffix =
                if s = "" then
                    suffix
                else
                    findLastSlash (String.suffix s 1) (if String.sub s 0 = #"/" then
                                                           String.suffix s 1
                                                       else
                                                           suffix)
        in
            findLastSlash url url
        end

    fun addId [r ::: {Type}] [[Attributes, Id] ~ r] (r : $([Attributes = {Url : string}] ++ r))
        : $([Id = string] ++ r) = r -- #Attributes ++ {Id = idFromUrl r.Attributes.Url}

    fun prefix inst = "https://" ^ inst ^ ".salesforce.com/services/data/v47.0/"
    fun record inst id = bless ("https://" ^ inst ^ ".lightning.force.com/lightning/r/" ^ urlencode id ^ "/view")

    functor Table(N : sig
                      val stable : stable
                      con fields :: {Type}
                      constraint [Id] ~ fields
                      val labels : $(map (fn _ => string) fields)
                      val jsons : $(map json fields)
                      val fl : folder fields

                      con relations :: {{Type}}
                      val rlabels : $(map (fn ts => string * $(map (fn _ => string) ts)) relations)
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
                String s => urlencode ("'" ^ escapeSingleQuotes s ^ "'")
              | Null => "NULL"
              | Field f => labelOf f
              | RField rf => rlabelOf rf
              | Binop (b, e1, e2) =>
                if allowsParens b then
                    "(" ^ formatExp e1 ^ ")"
                    ^ formatBinop b
                    ^ "(" ^ formatExp e2 ^ ")"
                else
                    formatExp e1 ^ "+"
                    ^ formatBinop b
                    ^ "+" ^ formatExp e2

        fun formatOrderBy1 (v, b) =
            labelOf v ^ "+" ^ (if b then "ASC" else "DESC")

        fun formatQuery [chosen] (q : query fields' relations chosen) =
            let
                val qu = "SELECT+" ^ q.Select labels rlabels ^ "+FROM+" ^ stable

                val qu = case q.Where of
                             None => qu
                           | Some e => qu ^ "+WHERE+" ^ formatExp e

                val qu = case q.OrderBy of
                             [] => qu
                           | ob1 :: obs =>
                             List.foldl (fn ob acc =>
                                            acc ^ "," ^ formatOrderBy1 ob)
                             (qu ^ "+ORDER+BY+" ^ formatOrderBy1 ob1) obs
            in
                qu
            end

        fun query [chosen] (fl : folder chosen) inst (q : query fields' relations chosen) =
            s <- api (bless (prefix inst ^ "query?q=" ^ @formatQuery q));
            return (@fromJson (@json_query_results (@json_record fl
                                                     (@mp [fn t => string * json t] [json]
                                                       (fn [t] (p : string * json t) => p.2)
                                                       fl (q.Json labels rlabels jsons rjsons))
                                                     (@mp [fn t => string * json t] [fn _ => string]
                                                       (fn [t] (p : string * json t) => p.1)
                                                       fl (q.Json labels rlabels jsons rjsons)))) s : query_results $chosen).Records

        fun insert inst vs =
            s <- apiPost (bless (prefix inst ^ "sobjects/" ^ stable ^ "/")) (vs labels jsons);
            case String.sindex {Needle = "\"message\"", Haystack = s} of
                None => return (fromJson s : success_response).Id
              | Some _ => error <xml>Salesforce {[stable]} insert failed: {[(fromJson s : error_response).Message]}</xml>

        fun update inst id vs =
            s <- apiPatch (bless (prefix inst ^ "sobjects/" ^ stable ^ "/" ^ Urls.urlencode id)) (vs labels jsons);
            case String.sindex {Needle = "\"message\"", Haystack = s} of
                None => return ()
              | Some _ => error <xml>Salesforce {[stable]} update failed: {[(fromJson s : error_response).Message]}</xml>
    end
end
