open Json
open Urls

type instance = string
val read_instance = _
val show_instance = _

functor ThreeLegged(M : sig
                        val https : bool
                        val sandbox : bool

                        val client_id : string
                        val client_secret : string
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

    fun auth url =
        authorize {ReturnTo = bless url}

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
                                               onclick={fn _ => redirect (url (auth (show cur)))}/></xml>}/>
        </xml>
end

type account_name = string
val read_account_name = _
val show_account_name = _

type account_id = string
val read_account_id = _
val show_account_id = _

type account = {
     Id : account_id,
     Nam : account_name,
     Website : option string
}

type new_account = {
     Nam : account_name,
     Website : option string
}
val _ : json new_account = json_record_withOptional {Nam = "Name"}
                           {Website = "Website"}

type new_contact = {
     FirstName : string,
     LastName : string,
     Account : option account_id,
     Email : option string
}
val _ : json new_contact = json_record_withOptional {FirstName = "FirstName",
                                                     LastName = "LastName"}
                                                    {Account = "AccountId",
                                                     Email = "Email"}

type contact_name = {
     FirstName : string,
     LastName : string
}

type attributes = {
     Url : string
}
val _ : json attributes = json_record {Url = "url"}

type account_query_result = {
     Attributes : attributes,
     Nam : account_name,
     Website : option string
}
val _ : json account_query_result = json_record {Nam = "Name", Attributes = "attributes", Website = "Website"}
                   
type account_query_results = {
     Records : list account_query_result
}
val _ : json account_query_results = json_record {Records = "records"}

type contact_query_result = {
     Attributes : attributes,
     FirstName : string,
     LastName : string,
     Account : option account_id,
     Email : option string
}
val _ : json contact_query_result = json_record_withOptional {FirstName = "FirstName",
                                                              LastName = "LastName",
                                                              Attributes = "attributes"}
                                                             {Account = "AccountId",
                                                              Email = "Email"}

type contact_query_results = {
     Records : list contact_query_result
}
val _ : json contact_query_results = json_record {Records = "records"}

type response = {
     Success : bool
}
val _ : json response = json_record {Success = "success"}
                             
functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Salesforce to use this feature.</xml>
          | Some tok => return tok
         
    fun api url =
        tok <- token;
        WorldFfi.get url (Some ("Bearer " ^ tok)) False

    fun apiPost url body =
        tok <- token;
        WorldFfi.post url (Some ("Bearer " ^ tok)) (Some "application/json") body

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
          
    structure Accounts = struct
        fun list inst =
            s <- api (bless (prefix inst ^ "query?q=SELECT+name,website+from+Account"));
            return (List.mp addId (fromJson s : account_query_results).Records)

        fun existsByName inst name =
            s <- api (bless (prefix inst ^ "query?q=SELECT+name,website+from+Account+where+name='" ^ urlencode name ^ "'"));
            return (List.length (fromJson s : account_query_results).Records = 1)

        fun lookupByName inst name =
            s <- api (bless (prefix inst ^ "query?q=SELECT+name,website+from+Account+where+name='" ^ urlencode name ^ "'"));
            case (fromJson s : account_query_results).Records of
                [] => return None
              | r :: [] => return (Some (addId r))
              | _ => error <xml>Multiple records returned from Salesforce account lookup by name</xml>

        fun insert inst ac =
            s <- apiPost (bless (prefix inst ^ "sobjects/Account/")) (toJson ac);
            if (fromJson s : response).Success then
                return ()
            else
                error <xml>Salesforce account insert failed.</xml>
    end

    structure Contacts = struct
        fun existsByName inst name =
            s <- api (bless (prefix inst ^ "query?q=SELECT+firstname,+lastname+from+Contact+where+firstname='" ^ urlencode name.FirstName ^ "'+and+lastname='" ^ urlencode name.LastName ^ "'"));
            return (List.length (fromJson s : contact_query_results).Records = 1)

        fun insert inst ct =
            debug ("Body " ^ show (toJson ct));
            s <- apiPost (bless (prefix inst ^ "sobjects/Contact/")) (toJson ct);
            if (fromJson s : response).Success then
                return ()
            else
                error <xml>Salesforce contact insert failed.</xml>
    end
end
