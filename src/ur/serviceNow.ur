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
    val instance : string
end

type incident = {
     Description : string
}
val _ : json incident = json_record {Description = "description"}
		
type result a = {
     Result : a
}
fun json_result [a] (_ : json a) : json (result a) = json_record {Result = "result"}

type tabl = {
     Nam : string
}
val _ : json tabl = json_record {Nam = "name"}

type reference = {
     Value : string
}
val _ : json reference = json_record {Value = "value"}

type tabl' = {
     Id : string,
     Nam : string,
     Parent : option reference
}
val _ : json tabl' = json_record_withOptional {Id = "sys_id", Nam = "name"}
					      {Parent = "super_class"}

type tabl'' = {
     Id : string,
     Nam : string
}
val _ : json tabl'' = json_record {Id = "sys_id", Nam = "name"}

type column = {
     Nam : string,
     Typ : string
}

type column' = {
     Nam : option string,
     Typ : option reference
}
val _ : json column' = json_record_withOptional {} {Nam = "element",
						    Typ = "internal_type"}

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into ServiceNow to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://" ^ instance ^ ".service-now.com/api/now/"

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("ServiceNow response: " ^ show v);
        return v

    fun api url =
        tok <- token;
        debug ("ServiceNow GET: " ^ prefix ^ url);
        logged (WorldFfi.get (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)

    structure Incidents = struct
        val list =
            s <- api "table/incident?sysparm_fields=description";
	    return (fromJson s : result (list incident)).Result
    end

    structure Tables = struct
        val list =
            s <- api "table/sys_db_object?sysparm_fields=name";
	    return (fromJson s : result (list tabl)).Result

	fun get tabl =
	    s <- api ("table/sys_db_object?sysparm_fields=sys_id,name,super_class&sysparm_query=super_classISNOTEMPTY^name=" ^ Urls.urlencode tabl);
	    raw <- return (fromJson s : result (list tabl')).Result;
	    case raw of
		t :: [] => return (Some t)
	      | [] => return None
	      | _ => error <xml>Surprising multiple results when looking up table "{[tabl]}" in ServiceNow.</xml>

	fun getById tid =
	    s <- api ("table/sys_db_object?sysparm_fields=sys_id,name&sysparm_query=sys_id=" ^ Urls.urlencode tid);
	    raw <- return (fromJson s : result (list tabl'')).Result;
	    case raw of
		t :: [] => return (Some t)
	      | [] => return None
	      | _ => error <xml>Surprising multiple results when looking up table #{[tid]} in ServiceNow.</xml>

        fun columnsWithoutInheritance tabl =
            s <- api ("table/sys_dictionary?sysparm_fields=element,internal_type&sysparm_query=name=" ^ Urls.urlencode tabl);
	    raw <- return (fromJson s : result (list column')).Result;
	    return (List.mapPartial (fn r =>
					name <- r.Nam;
					typ <- r.Typ;
					if name = "" then
					    None
					else
					    return {Nam = name, Typ = typ.Value}) raw)

	fun columns tabl =
	    cs <- columnsWithoutInheritance tabl;
	    t <- get tabl;
	    case t of
		None => return cs
	      | Some t =>
		case t.Parent of
		    None => return cs
		  | Some {Value = p} =>
		    p <- getById p;
		    case p of
			None => return cs
		      | Some p =>
			cs' <- columns p.Nam;
			return (List.append cs cs')
    end
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

                        val authorize_url = bless ("https://" ^ instance ^ ".service-now.com/oauth_auth.do")
                        val access_token_url = bless ("https://" ^ instance ^ ".service-now.com/oauth_token.do")

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
