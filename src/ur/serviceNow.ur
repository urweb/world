open Json

structure Scope = struct
    type t = Scopes.t []
    val empty = Scopes.empty
    val union = Scopes.union
    val toString = Scopes.toString {}

    val readonly = Scopes.disjoint empty
end

signature AUTH = sig
    val token : transaction (option string)
    val instance : transaction string
end

type incident = {
     Description : string
}
val _ : json incident = json_record {Description = "description"}

type result a = {
     Result : a
}
fun json_result [a] (_ : json a) : json (result a) = json_record {Result = "result"}

type table_name = {
     Nam : string
}
val _ : json table_name = json_record {Nam = "name"}

type reference = {
     Value : string
}
val _ : json reference = json_record {Value = "value"}

type rawColumn = {
     Nam : option string,
     Typ : option reference,
     Dis : option string
}

val _ : json rawColumn = json_record_withOptional {} {Nam = "element",
                                                      Typ = "internal_type",
                                                      Dis = "display"}

(* Bools seem to come back from ServiceNow as strings that are either "true" or "false" *)
fun unRawColumn (r : rawColumn) =
    let fun stringToBool s =
            if s = "true" then Some True
            else if s = "false" then Some False
            else None
        val dis = (x <- Option.mp stringToBool r.Dis; x) in
    name <- r.Nam;
    typ <- r.Typ;
    if name = "" then
        None
    else
        return {Nam = name, Typ = typ.Value, Dis = dis}
    end

type column = {
     Nam : string,
     Typ : string
}

type full_table = {
     Columns : list column,
     DisplayColumn : option string
}

(* This should probably be upstreamed. *)
fun oAlt o1 o2 = case o1 of Some _ => o1 | None => o2

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into ServiceNow to use this feature.</xml>
          | Some tok => return tok

    val prefix =
        instance <- instance;
        return ("https://" ^ instance ^ ".service-now.com/api/now/")

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("ServiceNow response: " ^ show v);
        return v

    fun api [a] (j : json a) (url : string) : transaction a =
        tok <- token;
        prefix <- prefix;
        debug ("ServiceNow GET: " ^ prefix ^ url);
        raw <- logged (WorldFfi.get (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False);
        return (fromJson raw : result a).Result

    structure Incidents = struct
        val list = @@api [list incident] _ "table/incident?sysparm_fields=description"
    end

    structure Tables = struct
        val list = @@api [list table_name] _ "table/sys_db_object?sysparm_fields=name"

        fun getParent tableName =
            tableList <- @@api
                [list {Parent : reference}]
                (@json_list <| json_record {Parent = "super_class"})
                ("table/sys_db_object?sysparm_fields=super_class&sysparm_query=super_classISNOTEMPTY^name=" ^ Urls.urlencode tableName);
            case tableList of
                [] => return None
              | t :: [] =>
                (parentTable <- @@api [list table_name] _ ("table/sys_db_object?sysparm_fields=name&sysparm_query=sys_id=" ^ Urls.urlencode t.Parent.Value);
                case parentTable of
                    pName :: [] => return <| Some pName.Nam
                  | [] => error <xml>ServiceNow said that table #"{[t.Parent.Value]}" exists, but it weirdly has no name.</xml>
                  | _ => error <xml>Surprising multiple results when looking up table #"{[t.Parent.Value]}" in ServiceNow.</xml>)
              | _ => error <xml>Surprising multiple results when looking up table "{[tableName]}" in ServiceNow.</xml>

        fun columnsWithoutInheritance tableName =
            raw <- @@api [list rawColumn] _ ("table/sys_dictionary?sysparm_fields=element,internal_type,display&sysparm_query=name=" ^ Urls.urlencode tableName);
            return <| List.mapPartial unRawColumn raw

        fun columnsWithDis tableName =
            cs <- columnsWithoutInheritance tableName;
            p <- getParent tableName;
            case p of
                None => return cs
              | Some pName =>
                cs' <- columnsWithDis pName;
                return (cs `List.append` cs')
                (* The order of appending matters here.  A child table may have
                   a different display field than a parent, so we should search
                   through them in order from child to parent.  See comment below
                   for more detail. *)

        (* From https://docs.servicenow.com/bundle/tokyo-platform-administration/page/administer/field-administration/task/t_SelectTheDisplayValue.html
           Reference fields look for the display value in the following order:
           a. A field with display=true in the system dictionary on the lowest sub-table for extended tables.
           b. A field with display=true in the system dictionary on the parent table.
           c. A field named name or u_name.
           d. The Created on field of the referenced record.
         *)

        fun columns tabl =
            cs <- columnsWithDis tabl;
            let val display =
                List.find (fn x => x.Dis = Some True) cs `oAlt`
                List.find (fn x => x.Nam = "name" || x.Nam = "u_name") cs `oAlt`
                List.find (fn x => x.Nam = "sys_created_on") cs
            in return {Columns = List.mp (fn x => x -- #Dis) cs,
                       DisplayColumn = Option.mp (fn x => x.Nam) display}
            end

    end

    structure Table = struct
        fun list [ts] (fl : folder ts) (labels : $(map (fn _ => string) ts))
                 (jsons : $(map json ts)) (tname : string) =
            fields <- return (@foldR [fn _ => string] [fn _ => string]
                              (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                                  (label : string) (acc : string) =>
                                  case acc of
                                      "" => label
                                    | _ => acc ^ "," ^ label)
                              "" fl labels);
            @@api
                [list $(map option ts)]
                (@json_list (@json_record_withOptional ! _ {} {} fl jsons labels))
                ("table/" ^ Urls.urlencode tname ^ "?sysparm_fields=" ^ fields)
    end
end

type settings = {
     ClientId : string,
     ClientSecret : string,
     Instance : string
}

functor ThreeLeggedDyn(M : sig
                           val https : bool

                           val settings : transaction settings

                           val onCompletion : transaction page
                    end) = struct
    open M

    val instance =
        settings <- settings;
        return settings.Instance

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

    open Oauth.MakeDyn(struct
                           open M

                           val settings =
                               settings <- settings;
                               url_base <- return ("https://" ^ settings.Instance ^ ".service-now.com/");
                               return {ClientId = settings.ClientId,
                                       ClientSecret = settings.ClientSecret,
                                       AuthorizeUrl = bless (url_base ^ "oauth_auth.do"),
                                       AccessTokenUrl = bless (url_base ^ "oauth_token.do")}

                           val scope = None
                           val hosted_domain = None

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
                           return <xml><button value="Log out of ServiceNow"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into ServiceNow"
                                               onclick={fn _ => redirect (url authorize)}/></xml>}/>
        </xml>
end

functor ThreeLegged(M : sig
                        val instance : string
                        val client_id : string
                        val client_secret : string
                        val https : bool

                        val scopes : Scope.t
                        val onCompletion : transaction page
                    end) = struct
    open M

    val instance = return instance

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
        case seconds of
            None => error <xml>Missing token expiration in OAuth response</xml>
          | Some seconds =>
            secret <- rand;
            tm <- now;
            dml (INSERT INTO secrets(Secret, Token, Expires)
                 VALUES ({[secret]}, {[tok]}, {[addSeconds tm (seconds * 3 / 4)]}));
            setCookie user {Value = secret,
                            Expires = None,
                            Secure = https}

    open Oauth.Make(struct
                        open M

                        val authorize_url = bless ("https://" ^ M.instance ^ ".service-now.com/oauth_auth.do")
                        val access_token_url = bless ("https://" ^ M.instance ^ ".service-now.com/oauth_token.do")

                        val withToken = withToken
                        val scope = None
                        val nameForScopeParameter = None
                        val parseTokenResponse = None
                        val hosted_domain = None
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

    val logout = clearCookie user

    val status =
        toko <- token;
        li <- source (Option.isSome toko);
        cur <- currentUrl;
        return <xml>
          <dyn signal={liV <- signal li;
                       if liV then
                           return <xml><button value="Log out of ServiceNow"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into ServiceNow"
                                               onclick={fn _ => redirect (url authorize)}/></xml>}/>
        </xml>
end
