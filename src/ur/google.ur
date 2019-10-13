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

type calendar_id = string
val show_calendar_id = _
val eq_calendar_id = _
                       
type calendar = {
     Id : calendar_id,
     Summary : string,
     Description : option string
}
val _ : json calendar = json_record_withOptional {Id = "id",
                                                  Summary = "summary"}
                                                 {Description = "description"}

type calendarList = {
     Items : list calendar
}
val _ : json calendarList = json_record {Items = "items"}

type event_id = string
val show_event_id = _
val eq_event_id = _

datatype when =
         Time of time
       | Date of {Year : int, Month : int, Day : int}

val show_when = mkShow (fn w =>
                           case w of
                               Time t => show t
                             | Date r => show r.Year ^ "/" ^ show r.Month ^ "/" ^ show r.Day)

datatype responseStatus =
         NeedsAction
       | Declined
       | Tentative
       | Accepted

type attendee = {
     AdditionalGuests : option int,
     Comment : option string,
     DisplayName : option string,
     Email : option string,
     Optional : option bool,
     Organizer : option bool,
     Resource : option bool,
     ResponseStatus : responseStatus,
     Self : option bool
}

type event = {
     Id : event_id,
     Summary : option string,
     Description : option string,
     Start : option when,
     End : option when,
     Attendees : option (list attendee)
}

type internal_attendee = {
     AdditionalGuests : option int,
     Comment : option string,
     DisplayName : option string,
     Email : option string,
     Optional : option bool,
     Organizer : option bool,
     Resource : option bool,
     ResponseStatus : string,
     Self : option bool
}
val _ : json internal_attendee = json_record_withOptional
                                     {ResponseStatus = "responseStatus"}
                                     {AdditionalGuests = "additionalGuests",
                                      Comment = "comment",
                                      DisplayName = "displayName",
                                      Email = "email",
                                      Optional = "optional",
                                      Organizer = "organizer",
                                      Resource = "resource",
                                      Self = "self"}

type internal_event = {
     Id : event_id,
     Summary : option string,
     Description : option string,
     Start : option {DateTime : option time, Date : option string},
     End : option {DateTime : option time, Date : option string},
     Attendees : option (list internal_attendee)
}
val _ : json {DateTime : option time, Date : option string} = json_record_withOptional {} {DateTime = "dateTime", Date = "date"}
val _ : json internal_event = json_record_withOptional
                                  {Id = "id"}
                                  {Summary = "summary",
                                   Description = "description",
                                   Start = "start",
                                   End = "end",
                                   Attendees = "attendees"}

type internal_newEvent = {
     Summary : option string,
     Description : option string,
     Start : option {DateTime : option time, Date : option string},
     End : option {DateTime : option time, Date : option string},
     Attendees : option (list internal_attendee)
}
val _ : json internal_newEvent = json_record_withOptional
                                     {}
                                     {Summary = "summary",
                                      Description = "description",
                                      Start = "start",
                                      End = "end",
                                      Attendees = "attendees"}

type events = {
     Items : list internal_event
}
val _ : json events = json_record {Items = "items"}

type newEvent = {
     Summary : option string,
     Description : option string,
     Start : option when,
     End : option when,
     Attendees : option (list attendee)
}

functor Calendar(M : sig
                     include S
                     val readonly : bool
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
                        open OauthP

                        val withToken = withToken
                        val scope = Some ("https://www.googleapis.com/auth/calendar"
                                          ^ if readonly then ".readonly" else "")
                    end)

    val logout = clearCookie user

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

    fun apiPost url body =
        tok <- token;
        WorldFfi.post url (Some ("Bearer " ^ tok)) (Some "application/json") body

    fun apiPut url body =
        tok <- token;
        WorldFfi.put url (Some ("Bearer " ^ tok)) (Some "application/json") body

    fun apiDelete url =
        tok <- token;
        WorldFfi.delete url (Some ("Bearer " ^ tok))

    structure Calendars = struct
        val list =
            s <- api (bless "https://www.googleapis.com/calendar/v3/users/me/calendarList");
            return (fromJson s : calendarList).Items
    end

    fun ingestWhen r =
        case r.DateTime of
            Some t => Time t
          | None =>
            case r.Date of
                Some s =>
                (case String.split s #"-" of
                     None => error <xml>Invalid date string "{[s]}" in Google API response</xml>
                   | Some (y, rest) =>
                     case String.split rest #"-" of
                         None => error <xml>Invalid date string "{[s]}" in Google API response</xml>
                       | Some (m, d) =>
                         case (read y, read m, read d) of
                             (Some y, Some m, Some d) => Date {Year = y, Month = m, Day = d}
                           | _ => error <xml>Invalid date string "{[s]}" in Google API response</xml>)
              | None => error <xml>Google API response contains an empty time</xml>
        
    fun ingestAttendee a =
        a -- #ResponseStatus
          ++ {ResponseStatus = case a.ResponseStatus of
                                   "needsAction" => NeedsAction
                                 | "declined" => Declined
                                 | "tentative" => Tentative
                                 | "accepted" => Accepted
                                 | s => error <xml>Bad Google Calendar response status "{[s]}"</xml>}

    fun ingestEvent e =
        e -- #Start -- #End -- #Attendees
          ++ {Start = Option.mp ingestWhen e.Start,
              End = Option.mp ingestWhen e.End,
              Attendees = Option.mp (List.mp ingestAttendee) e.Attendees}

    fun paddedInt padding n =
        let
            val s = show n
            fun zeroes n =
                if n <= 0 then
                    ""
                else
                    "0" ^ zeroes (n - 1)
        in
            zeroes (padding - String.length s) ^ s
        end

    fun excreteWhen w =
        case w of
            Time t => {DateTime = Some t, Date = None}
          | Date r => {DateTime = None, Date = Some (paddedInt 4 r.Year ^ "-" ^ paddedInt 2 r.Month ^ "-" ^ paddedInt 2 r.Day)}

    fun excreteAttendee a =
        a -- #ResponseStatus
          ++ {ResponseStatus = case a.ResponseStatus of
                                   NeedsAction => "needsAction"
                                 | Declined => "declined"
                                 | Tentative => "tentative"
                                 | Accepted => "accepted"}

    fun excreteEvent e =
        e -- #Start -- #End -- #Attendees
          ++ {Start = Option.mp excreteWhen e.Start,
              End = Option.mp excreteWhen e.End,
              Attendees = Option.mp (List.mp excreteAttendee) e.Attendees}

    structure Events = struct
        fun list cid =
            s <- api (bless ("https://www.googleapis.com/calendar/v3/calendars/" ^ cid ^ "/events"));
            return (List.mp ingestEvent (fromJson s : events).Items)

        fun insert cid e =
            if readonly then
                error <xml>Google Calendar: attempt to <tt>Events.insert</tt> in read-only mode</xml>
            else
                s <- apiPost (bless ("https://www.googleapis.com/calendar/v3/calendars/" ^ cid ^ "/events")) (toJson (excreteEvent e));
                return (ingestEvent (fromJson s))

        fun update cid e =
            if readonly then
                error <xml>Google Calendar: attempt to <tt>Events.update</tt> in read-only mode</xml>
            else
                s <- apiPut (bless ("https://www.googleapis.com/calendar/v3/calendars/" ^ cid ^ "/events/" ^ e.Id)) (toJson (excreteEvent (e -- #Id)));
                return (ingestEvent (fromJson s))

        fun delete cid eid =
            if readonly then
                error <xml>Google Calendar: attempt to <tt>Events.delete</tt> in read-only mode</xml>
            else
                Monad.ignore (apiDelete (bless ("https://www.googleapis.com/calendar/v3/calendars/" ^ cid ^ "/events/" ^ eid)))
    end
end