open Json

signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

signature SS = sig
    val service_account : string
    val private_key : string
    val impersonated_user : string
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
val read_calendar_id = _
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

datatype updatesMode =
         All
       | ExternalOnly
       | NoneUpdates
val _ = mkShow (fn m =>
                   case m of
                       All => "all"
                     | ExternalOnly => "externalOnly"
                     | NoneUpdates => "none")

signature CALENDAR_AUTH = sig
    val readonly : bool
    val token : transaction (option string)
end

functor Calendar(M : CALENDAR_AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Google Calendar to use this feature.</xml>
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
        fun list cid bounds =
            url <- return ("https://www.googleapis.com/calendar/v3/calendars/" ^ cid ^ "/events");
            url <- return (case bounds.Min of
                               None => url
                             | Some min => url ^ "?timeMin=" ^ rfc3339_out min);
            url <- return (case bounds.Max of
                               None => url
                             | Some max => url ^ (if Option.isNone bounds.Min then "?" else "&") ^ "timeMax=" ^ rfc3339_out max);
            s <- api (bless url);
            return (List.mp ingestEvent (fromJson s : events).Items)

        fun insert cid flags e =
            if readonly then
                error <xml>Google Calendar: attempt to <tt>Events.insert</tt> in read-only mode</xml>
            else
                url <- return ("https://www.googleapis.com/calendar/v3/calendars/" ^ cid ^ "/events");
                url <- return (case flags.SendUpdates of
                                   None => url
                                 | Some m => url ^ "?sendUpdates=" ^ show m);
                s <- apiPost (bless url) (toJson (excreteEvent e));
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

functor CalendarThreeLegged(M : sig
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

    val token =
        c <- getCookie user;
        case c of
            None => return None
          | Some n =>
            oneOrNoRowsE1 (SELECT (secrets.Token)
                           FROM secrets
                           WHERE secrets.Secret = {[n]}
                             AND secrets.Expires > CURRENT_TIMESTAMP)

    val status =
        li <- loggedIn;
        li <- source li;
        cur <- currentUrl;
        return <xml>
          <dyn signal={liV <- signal li;
                       if liV then
                           return <xml><button value="Log out of Google Calendar"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into Google Calendar"
                                               onclick={fn _ => redirect (url (authorize {ReturnTo = cur}))}/></xml>}/>
        </xml>
end

fun base64url_encode' (getChar : int -> char) (len : int) =
    let
        fun char n =
            String.str (Char.fromInt (if n < 0 then
                                          error <xml>Negative character to base64 encode</xml>
                                      else if n < 26 then
                                          Char.toInt #"A" + n
                                      else if n < 52 then
                                          Char.toInt #"a" + (n - 26)
                                      else if n < 62 then
                                          Char.toInt #"0" + (n - 52)
                                      else if n = 62 then
                                          Char.toInt #"-"
                                      else if n = 63 then
                                          Char.toInt #"_"
                                      else
                                          error <xml>Invalid base64 digit</xml>))

        fun ch j =
            let
                val n = Char.toInt (getChar j)
            in
                if n < 0 then
                    n + 256
                else
                    n
            end

        fun bytes i acc =
            if i >= len then
                acc
            else if i = len - 1 then
                let
                    val n = ch i * 16
                in
                    acc
                    ^ char (n / 64)
                    ^ char (n % 64)
                    ^ "=="
                end
            else if i = len - 2 then
                let
                    val n1 = ch i
                    val n2 = ch (i + 1)
                    val n = n1 * (256 * 4) + n2 * 4
                in
                    acc
                    ^ char (n / (64 * 64))
                    ^ char (n / 64 % 64)
                    ^ char (n % 64)
                    ^ "="
                end
            else
                let
                    val n1 = ch i
                    val n2 = ch (i + 1)
                    val n3 = ch (i + 2)
                    val n = n1 * (256 * 256) + n2 * 256 + n3
                in
                    bytes (i + 3) (acc
                                   ^ char (n / (64 * 64 * 64))
                                   ^ char (n / (64 * 64) % 64)
                                   ^ char (n / 64 % 64)
                                   ^ char (n % 64))
                end
    in
        bytes 0 ""
    end

fun base64url_encode s = base64url_encode' (String.sub s) (String.length s)
fun base64url_encode_signature s = base64url_encode' (WorldFfi.byte s) (WorldFfi.length s)

type jwt_header = {
     Alg : string,
     Typ : string
}
val _ : json jwt_header = json_record {Alg = "alg",
                                       Typ = "typ"}
val jwt_header = {Alg = "RS256",
                  Typ = "JWT"}

type jwt_claim_set = {
     Iss : string,
     Sub : string,
     Scope : string,
     Aud : string,
     Exp : int,
     Iat : int
}
val _ : json jwt_claim_set = json_record {Iss = "iss",
                                          Sub = "sub",
                                          Scope = "scope",
                                          Aud = "aud",
                                          Exp = "exp",
                                          Iat = "iat"}

type jwt_response = {
     AccessToken : string,
     ExpiresIn : int
}
val _ : json jwt_response = json_record {AccessToken = "access_token",
                                         ExpiresIn = "expires_in"}

functor CalendarTwoLegged(M : sig
                              include SS
                              val readonly : bool
                          end) = struct
    open M

    table mytoken : { Token : string,
                      Expires : time }

    task periodic 60 = fn () =>
                          tm <- now;
                          dml (DELETE FROM mytoken
                               WHERE Expires < {[addSeconds tm (-60)]})

    val token =
        tokopt <- oneOrNoRowsE1 (SELECT (mytoken.Token)
                                 FROM mytoken
                                 WHERE mytoken.Expires > CURRENT_TIMESTAMP);
        case tokopt of
            Some tok => return (Some tok)
          | None =>
            tm <- now;
            header <- return (toJson jwt_header);
            clset <- return (toJson {Iss = service_account,
                                     Sub = impersonated_user,
                                     Scope = "https://www.googleapis.com/auth/calendar"
                                             ^ (if readonly then ".readonly" else ""),
                                     Aud = "https://oauth2.googleapis.com/token",
                                     Exp = toSeconds (addSeconds tm (60 * 60)),
                                     Iat = toSeconds tm});
            header_clset <- return (base64url_encode header ^ "." ^ base64url_encode clset);
            signed <- return (WorldFfi.sign private_key header_clset);
            assertion <- return (header_clset ^ "." ^ base64url_encode_signature signed);
            resp <- WorldFfi.post (bless "https://oauth2.googleapis.com/token") None
                                  (Some "application/x-www-form-urlencoded")
                                  ("grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=" ^ assertion);
            resp <- return (fromJson resp : jwt_response);
            dml (DELETE FROM mytoken WHERE TRUE);
            dml (INSERT INTO mytoken(Token, Expires)
                 VALUES ({[resp.AccessToken]}, {[addSeconds tm resp.ExpiresIn]}));
            return (Some resp.AccessToken)
end
