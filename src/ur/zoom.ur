open Json

signature S = sig
    val api_key : string
    val api_secret : string
end

signature AUTH = sig
    val token : transaction (option string)
end

datatype meeting_type =
         Instant
       | Scheduled
       | RecurringUnfixed
       | RecurringFixed
val _ : json meeting_type = json_derived
                                (fn x =>
                                    case x of
                                        1 => Instant
                                      | 2 => Scheduled
                                      | 3 => RecurringUnfixed
                                      | 8 => RecurringFixed
                                      | _ => error <xml>Bad Zoom meeting type {[x]}</xml>)
                                (fn x =>
                                    case x of
                                        Instant => 1
                                      | Scheduled => 2
                                      | RecurringUnfixed => 3
                                      | RecurringFixed => 8)

datatype recurrence_type =
         Daily
       | Weekly
       | Monthly
val _ : json recurrence_type = json_derived
                                   (fn x =>
                                       case x of
                                           1 => Daily
                                         | 2 => Weekly
                                         | 3 => Monthly
                                         | _ => error <xml>Bad Zoom recurrence type {[x]}</xml>)
                                   (fn x =>
                                       case x of
                                           Daily => 1
                                         | Weekly => 2
                                         | Monthly => 3)

datatype monthly_week =
         Last
       | First
       | Second
       | Third
       | Fourth
val _ : json monthly_week = json_derived
                                (fn x =>
                                    case x of
                                        -1 => Last
                                      | 1 => First
                                      | 2 => Second
                                      | 3 => Third
                                      | 4 => Fourth
                                      | _ => error <xml>Bad Zoom monthly week {[x]}</xml>)
                                (fn x =>
                                    case x of
                                        Last => -1
                                      | First => 1
                                      | Second => 2
                                      | Third => 3
                                      | Fourth => 4)

val _ : json Datetime.day_of_week = json_derived
                                        (fn x => Datetime.intToDayOfWeek (x - 1))
                                        (fn x => 1 + Datetime.dayOfWeekToInt x)

type recurrence = {
     Typ : recurrence_type,
     RepeatInterval : int,
     WeeklyDays : option (list Datetime.day_of_week),
     MonthlyDay : option int,
     MonthlyWeek : option monthly_week,
     MonthlyWeekDay : option Datetime.day_of_week,
     EndTimes : option int,
     EndDateTime : option time
}
val _ : json recurrence = json_record_withOptional
                              {Typ = "type",
                               RepeatInterval = "repeat_interval"}
                              {WeeklyDays = "weekly_days",
                               MonthlyDay = "monthly_day",
                               MonthlyWeek = "monthly_week",
                               MonthlyWeekDay = "monthly_week_day",
                               EndTimes = "end_times",
                               EndDateTime = "end_date_time"}

datatype approval_type =
         Automatically
       | Manually
       | NoRegistrationRequired
val _ : json approval_type = json_derived
                                 (fn x =>
                                     case x of
                                         0 => Automatically
                                       | 1 => Manually
                                       | 2 => NoRegistrationRequired
                                       | _ => error <xml>Bad Zoom approval type {[x]}</xml>)
                                 (fn x =>
                                     case x of
                                         Automatically => 0
                                       | Manually => 1
                                       | NoRegistrationRequired => 2)

datatype registration_type =
         Once
       | Each
       | OnceForSeveral
val _ : json registration_type = json_derived
                                 (fn x =>
                                     case x of
                                         1 => Once
                                       | 2 => Each
                                       | 3 => OnceForSeveral
                                       | _ => error <xml>Bad Zoom registration type {[x]}</xml>)
                                 (fn x =>
                                     case x of
                                         Once => 1
                                       | Each => 2
                                       | OnceForSeveral => 3)
         
datatype audio =
         Both
       | Telephony
       | Voip
val _ : json audio = json_derived
                         (fn x => case x of
                                      "both" => Both
                                    | "telephony" => Telephony
                                    | "voip" => Voip
                                    | _ => error <xml>Bad Zoom audio setting {[x]}</xml>)
                         (fn x =>
                             case x of
                                 Both => "both"
                               | Telephony => "telephony"
                               | Voip => "voip")

datatype auto_recording =
         Local
       | Cloud
       | NoRecording
val _ : json auto_recording = json_derived
                                  (fn x =>
                                      case x of
                                          "local" => Local
                                        | "cloud" => Cloud
                                        | "none" => NoRecording
                                        | _ => error <xml>Bad Zoom auto-recording setting {[x]}</xml>)
                                  (fn x =>
                                      case x of
                                          Local => "local"
                                        | Cloud => "cloud"
                                        | NoRecording => "none")

val _ : json (list string) = json_derived
                             (fn x =>
                                 let
                                     fun parse s =
                                         case s of
                                             "" => []
                                           | _ =>
                                             case String.split s #"," of
                                                 None => s :: []
                                               | Some (s1, s2) => s1 :: parse s2
                                 in
                                     parse x
                                 end)
                             (fn x =>
                                 let
                                     fun unparse ls =
                                         case ls of
                                             [] => ""
                                           | s :: [] => s
                                           | s :: ls' => s ^ "," ^ unparse ls'
                                 in
                                     unparse x
                                 end)

type meeting_settings = {
     HostVideo : option bool,
     ParticipantVideo : option bool,
     CnMeeting : option bool,
     InMeeting : option bool,
     JoinBeforeHost : option bool,
     MuteUponEntry : option bool,
     UsePmi : option bool,
     ApprovalType : option approval_type,
     RegistrationType : option registration_type,
     Audio : option audio,
     AutoRecording : option auto_recording,
     EnforceLogin : option bool,
     EnforceLoginDomains : option (list string),
     AlternativeHosts : option (list string),
     CloseRegistration : option bool,
     WaitingRoom : option bool,
     GlobalDialInCountries : option (list string),
     ContactName : option string,
     ContactEmail : option string,
     RegistrantsEmailNotification : option bool,
     MeetingAuthentication : option bool,
     AuthenticationOption : option string,
     AuthenticationDomains : option (list string)
}
val _ : json meeting_settings = json_record_withOptional
                                    {}
                                    {HostVideo = "host_video",
                                     ParticipantVideo = "participant_video",
                                     CnMeeting = "cn_meeting",
                                     InMeeting = "in_meeting",
                                     JoinBeforeHost = "join_before_host",
                                     MuteUponEntry = "mute_upon_entry",
                                     UsePmi = "use_pmi",
                                     ApprovalType = "approval_type",
                                     RegistrationType = "registration_type",
                                     Audio = "audio",
                                     AutoRecording = "auto_recording",
                                     EnforceLogin = "enforce_login",
                                     EnforceLoginDomains = "enforce_login_domains",
                                     AlternativeHosts = "alternative_hosts",
                                     CloseRegistration = "close_registration",
                                     WaitingRoom = "waiting_room",
                                     GlobalDialInCountries = "global_dial_in_countries",
                                     ContactName = "contact_name",
                                     ContactEmail = "contact_email",
                                     RegistrantsEmailNotification = "registrants_email_notification",
                                     MeetingAuthentication = "meeting_authentication",
                                     AuthenticationOption = "authentication_option",
                                     AuthenticationDomains = "authentication_domains"}
         
type meeting = {
     Uuid : option string,
     Id : option int,
     HostId : option string,
     Topic : string,
     Typ : meeting_type,
     StartTime : option time,
     Duration : option int,
     Timezone : option string,
     Password : option string,
     Agenda : option string,
     CreatedAt : option time,
     JoinUrl : option string,
     Recurrence : option recurrence,
     Settings : option meeting_settings
}
val _ : json meeting = json_record_withOptional
                           {Topic = "topic",
                            Typ = "type"}
                           {Uuid = "uuid",
                            Id = "id",
                            HostId = "host_id",
                            StartTime = "start_time",
                            Duration = "duration",
                            Timezone = "timezone",
                            Password = "password",
                            Agenda = "agenda",
                            CreatedAt = "created_at",
                            JoinUrl = "join_url",
                            Recurrence = "recurrence",
                            Settings = "settings"}

type meetings_response = {
     Meetings : list meeting
}
val _ : json meetings_response = json_record {Meetings = "meetings"}
    
type room = {
     Id : string,
     Nam : string,
     ActivationCode : string,
     Status : string
}
val _ : json room = json_record {Id = "id",
                                 Nam = "name",
                                 ActivationCode = "activation_code",
                                 Status = "status"}
                 
type rooms_response = {
     Rooms : list room
}
val _ : json rooms_response = json_record {Rooms = "rooms"}

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Google Calendar to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://api.zoom.us/v2/"
                        
    fun api url =
        tok <- token;
        WorldFfi.get (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False

    fun apiPost url body =
        tok <- token;
        WorldFfi.post (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) (Some "application/json") body

    structure Meetings = struct
        val list =
            s <- api "users/me/meetings";
            return (fromJson s : meetings_response).Meetings

        fun create x =
            s <- apiPost "users/me/meetings" (toJson x);
            return (fromJson s)
    end

    structure Rooms = struct
        val list =
            s <- api "rooms";
            return (fromJson s : rooms_response).Rooms
    end
end

type jwt_header = {
     Alg : string,
     Typ : string
}
val _ : json jwt_header = json_record {Alg = "alg",
                                       Typ = "typ"}
val jwt_header = {Alg = "HS256",
                  Typ = "JWT"}

type jwt_claim_set = {
     Iss : string,
     Exp : int,
     Iat : int
}
val _ : json jwt_claim_set = json_record {Iss = "iss",
                                          Exp = "exp",
                                          Iat = "iat"}

type jwt_response = {
     AccessToken : string,
     ExpiresIn : int
}
val _ : json jwt_response = json_record {AccessToken = "access_token",
                                         ExpiresIn = "expires_in"}

functor TwoLegged(M : S) = struct
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
            exp <- return (addSeconds tm (60 * 60));
            clset <- return (toJson {Iss = api_key,
                                     Exp = toSeconds exp,
                                     Iat = toSeconds tm});
            header_clset <- return (Urls.base64url_encode header ^ "." ^ Urls.base64url_encode clset);
            signed <- return (WorldFfi.sign_hs256 api_secret header_clset);
            token <- return (header_clset ^ "." ^ Urls.base64url_encode_signature signed);
            dml (DELETE FROM mytoken WHERE TRUE);
            dml (INSERT INTO mytoken(Token, Expires)
                 VALUES ({[token]}, {[exp]}));
            return (Some token)
end
