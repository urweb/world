open Json

signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

type profile = { ResourceName : string }

val json_profile : json profile =
    json_record {ResourceName = "resourceName"}

structure OauthP = struct
    val authorize_url = bless "https://accounts.google.com/o/oauth2/auth"
    val access_token_url = bless "https://oauth2.googleapis.com/token"
end
    
functor Login(M : S) = struct
    open M

    table secrets : { ResourceName : string,
                      Secret : int }
      PRIMARY KEY ResourceName
                    
    cookie user : { ResourceName : string, Secret : int }

    fun withToken {Token = tok, ...} =
        profile <- WorldFfi.get (bless "https://people.googleapis.com/v1/people/me?personFields=emailAddresses") (Some ("Bearer " ^ tok));
        (profile : profile) <- return (Json.fromJson profile);
        secret <- oneOrNoRowsE1 (SELECT (secrets.Secret)
                                 FROM secrets
                                 WHERE secrets.ResourceName = {[profile.ResourceName]});
        secret <- (case secret of
                       Some secret => return secret
                     | None =>
                       secret <- rand;
                       dml (INSERT INTO secrets(ResourceName, Secret)
                            VALUES ({[profile.ResourceName]}, {[secret]}));
                       return secret);

        setCookie user {Value = {ResourceName = profile.ResourceName, Secret = secret},
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M
                        open OauthP

                        val withToken = withToken
                        val scope = Some "profile"
                    end)

    val whoami =
        c <- getCookie user;
        case c of
            None => return None
          | Some r =>
            ok <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM secrets
                            WHERE secrets.ResourceName = {[r.ResourceName]}
                              AND secrets.Secret = {[r.Secret]});
            if ok then
                return (Some r.ResourceName)
            else
                return None

    val logout = clearCookie user
end

type message_id = string
val show_message_id = _
val eq_message_id = _

type thread_id = string
val show_thread_id = _
val eq_thread_id = _

type message = {
     Id : message_id,
     ThreadId : thread_id
}

type messages = {
     Messages : list message
}

val _ : json message = json_record {Id = "id",
                                    ThreadId = "threadId"}
val _ : json messages = json_record {Messages = "messages"}

type label_id = string
val show_label_id = _

type header = {
     Nam : string,
     Value : string
}
val _ : json header = json_record {Nam = "name",
                                   Value = "value"}

type payload_metadata = {
     MimeType : string,
     Headers : list header
}
val _ : json payload_metadata = json_record {MimeType = "mimeType",
                                             Headers = "headers"}

type history_id = string
val show_history_id = _
val ord_history_id = mkOrd {Lt = fn a b => (readError a : int) < readError b,
                            Le = fn a b => (readError a : int) <= readError b}
val inj_history_id = _

type message_metadata = {
     Id : message_id,
     ThreadId : thread_id,
     LabelIds : list label_id,
     Snippet : string,
     HistoryId : history_id,
     InternalDate : string,
     Payload : payload_metadata,
     SizeEstimate : int
}
val _ : json message_metadata = json_record {Id = "id",
                                             ThreadId = "threadId",
                                             LabelIds = "labelIds",
                                             Snippet = "snippet",
                                             HistoryId = "historyId",
                                             InternalDate = "internalDate",
                                             Payload = "payload",
                                             SizeEstimate = "sizeEstimate"}

type hmessage = {
     Id : message_id,
     ThreadId : thread_id,
     LabelIds : list string
}
val _ : json hmessage = json_record {Id = "id",
                                     ThreadId = "threadId",
                                     LabelIds = "labelIds"}
                                
type ma_item = {
     Message : hmessage
}
val _ : json ma_item = json_record {Message = "message"}
                                
type history_item = {
     MessagesAdded : list ma_item
}
val _ : json history_item = json_record {MessagesAdded = "messagesAdded"}
                                
type history = {
     History : list history_item,
     HistoryId : history_id
}
val _ : json history = json_record {History = "history", HistoryId = "historyId"}

type gmail_profile = {
     EmailAddress : string
}
val _ : json gmail_profile = json_record {EmailAddress = "emailAddress"}
                       
functor Gmail(M : S) = struct
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
    cookie email : string

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
                        open OauthP

                        val withToken = withToken
                        val scope = Some "https://www.googleapis.com/auth/gmail.readonly"
                    end)

    val logout = clearCookie user; clearCookie email
    val loggedIn =
        v <- getCookie user;
        case v of
            None => return False
          | Some secret => oneRowE1 (SELECT COUNT( * ) > 0
                                     FROM secrets
                                     WHERE secrets.Secret = {[secret]}
                                       AND secrets.Expires > CURRENT_TIMESTAMP)
                 
    val token =
        c <- getCookie user;
        case c of
            None => error <xml>You must be logged into Google to use this feature.</xml>
          | Some n =>
            tokopt <- oneOrNoRowsE1 (SELECT (secrets.Token)
                                     FROM secrets
                                     WHERE secrets.Secret = {[n]}
                                       AND secrets.Expires > CURRENT_TIMESTAMP);
            case tokopt of
                None => error <xml>You must be logged into Google to use this feature.</xml>
              | Some tok => return tok

    fun api url =
        tok <- token;
        WorldFfi.get url (Some ("Bearer " ^ tok))

    val emailAddress =
        r <- getCookie email;
        case r of
            Some r => return r
          | None =>
            s <- api (bless "https://www.googleapis.com/gmail/v1/users/me/profile");
            r <- return (fromJson s : gmail_profile).EmailAddress;
            setCookie email {Value = r,
                             Expires = None,
                             Secure = https};
            return r
        
    val messages =
        s <- api (bless "https://www.googleapis.com/gmail/v1/users/me/messages");
        return (fromJson s : messages).Messages

    fun messageMetadata id =
        s <- api (bless ("https://www.googleapis.com/gmail/v1/users/me/messages/" ^ id ^ "?format=metadata"));
        return (fromJson s)

    fun history hid =
        s <- api (bless ("https://www.googleapis.com/gmail/v1/users/me/history?historyTypes=messageAdded&startHistoryId=" ^ hid));
        case String.sindex {Haystack = s, Needle = "\"history\""} of
            None => return ([], None)
          | Some _ =>
            (h : history) <- return (fromJson s);
            ms <- return (List.foldl (fn hi ms => List.append (List.mp (fn r => r.Message) hi.MessagesAdded) ms) [] h.History);
            return (List.mapPartial (fn m => if List.mem "DRAFT" m.LabelIds then None else Some (m -- #LabelIds)) ms, Some h.HistoryId)

    fun ofThread tid =
        addr <- emailAddress;
        return (bless ("https://mail.google.com/mail?authuser=" ^ addr ^ "#all/" ^ tid))
end
