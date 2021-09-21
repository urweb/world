open Json

structure Scope = struct
    type t = Scopes.t [MeetingRead, MeetingWrite, WebinarRead, WebinarWrite]
    val empty = Scopes.empty
    val union = Scopes.union
    val toString = Scopes.toString {MeetingRead = "meeting:read",
                                    MeetingWrite = "meeting:write",
                                    WebinarRead = "webinar:read",
                                    WebinarWrite = "webinar:write"}

    val meetingRead = Scopes.one [#MeetingRead]
    val meetingWrite = Scopes.one [#MeetingWrite]
    val webinarRead = Scopes.one [#WebinarRead]
    val webinarWrite = Scopes.one [#WebinarWrite]

    val readonly = Scopes.disjoint (union meetingWrite webinarWrite)
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
     WeeklyDays : option string,
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

datatype global_dial_in_type =
         Toll
       | Tollfree
val _ : json global_dial_in_type = json_derived
                                   (fn x =>
                                       case x of
                                           "toll" => Toll
                                         | "tollfree" => Tollfree
                                         | _ => error <xml>Bad Zoom global-dial-in type {[x]}</xml>)
                                   (fn x =>
                                       case x of
                                           Toll => "toll"
                                         | Tollfree => "tollfree")

type global_dial_in_number = {
     Country : string,
     CountryName : string,
     City : option string,
     Number : string,
     Typ : global_dial_in_type
}
val _ : json global_dial_in_number = json_record_withOptional {Country = "country",
                                                               CountryName = "country_name",
                                                               Number = "number",
                                                               Typ = "type"}
                                                              {City = "city"}

type global_dial_in_country = {
     CountryName : string
}
val _ : json global_dial_in_country = json_derived (fn s => {CountryName = s})
                                                   (fn r => r.CountryName)

type meeting_settings = {
     HostVideo : option bool,
     ParticipantVideo : option bool,
     CnMeeting : option bool,
     InMeeting : option bool,
     JoinBeforeHost : option bool,
     MuteUponEntry : option bool,
     Watermark : option bool,
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
     GlobalDialInCountries : option (list global_dial_in_country),
     GlobalDialInNumbers : option (list global_dial_in_number),
     ContactName : option string,
     ContactEmail : option string,
     RegistrantsConfirmationEmail : option bool,
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
                                     Watermark = "watermark",
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
                                     GlobalDialInNumbers = "global_dial_in_numbers",
                                     ContactName = "contact_name",
                                     ContactEmail = "contact_email",
                                     RegistrantsConfirmationEmail = "registrants_confirmation_email",
                                     RegistrantsEmailNotification = "registrants_email_notification",
                                     MeetingAuthentication = "meeting_authentication",
                                     AuthenticationOption = "authentication_option",
                                     AuthenticationDomains = "authentication_domains"}

datatype meeting_status =
         Waiting
       | Started
       | Finished
val _ : json meeting_status = json_derived
                                  (fn x =>
                                      case x of
                                          "waiting" => Waiting
                                        | "started" => Started
                                        | "finished" => Finished
                                        | _ => error <xml>Bad Zoom meeting status {[x]}</xml>)
                                  (fn x =>
                                      case x of
                                          Waiting => "waiting"
                                        | Started => "started"
                                        | Finished => "finished")

type meeting = {
     Uuid : option string,
     Id : option int,
     HostId : option string,
     Topic : string,
     Typ : meeting_type,
     Status : option meeting_status,
     StartTime : option time,
     Duration : option int,
     Timezone : option string,
     Password : option string,
     H323Password : option string,
     Pmi : option int,
     Agenda : option string,
     CreatedAt : option time,
     StartUrl : option string,
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
                            Status = "status",
                            StartTime = "start_time",
                            Duration = "duration",
                            Timezone = "timezone",
                            Password = "password",
                            H323Password = "h323_password",
                            Pmi = "pmi",
                            Agenda = "agenda",
                            CreatedAt = "created_at",
                            StartUrl = "start_url",
                            JoinUrl = "join_url",
                            Recurrence = "recurrence",
                            Settings = "settings"}

datatype webinar_type =
         Webinar
       | WebinarRecurringUnfixed
       | WebinarRecurringFixed
val _ : json webinar_type = json_derived
                                (fn x =>
                                    case x of
                                        5 => Webinar
                                      | 6 => WebinarRecurringUnfixed
                                      | 9 => WebinarRecurringFixed
                                      | _ => error <xml>Bad Zoom webinar status {[x]}</xml>)
                                (fn x =>
                                    case x of
                                        Webinar => 5
                                      | WebinarRecurringUnfixed => 6
                                      | WebinarRecurringFixed => 9)

type webinar_settings = {
     HostVideo : option bool,
     PanelistsVideo : option bool,
     PracticeSession : option bool,
     HdVideo : option bool,
     ApprovalType : option approval_type,
     RegistrationType : option registration_type,
     Audio : option audio,
     AutoRecording : option auto_recording,
     EnforceLogin : option bool,
     EnforceLoginDomains : option (list string),
     AlternativeHosts : option (list string),
     CloseRegistration : option bool,
     ShowShareButton : option bool,
     AllowMultipleDevices : option bool,
     OnDemand : option bool,
     GlobalDialInCountries : option (list global_dial_in_country),
     ContactName : option string,
     ContactEmail : option string,
     RegistrantsConfirmationEmail : option bool,
     RegistrantsRestrictNumber : option int,
     NotifyRegistrantgs : option bool,
     PostWebinarSurvey : option bool,
     SurveyUrl : option string,
     RegistrantsEmailNotification : option bool,
     MeetingAuthentication : option bool,
     AuthenticationOption : option string,
     AuthenticationDomains : option (list string),
     AuthenticationName : option string
}
val _ : json webinar_settings = json_record_withOptional {}
                                {HostVideo = "host_video",
                                 PanelistsVideo = "panelists_video",
                                 PracticeSession = "practice_session",
                                 HdVideo = "hd_video",
                                 ApprovalType = "approval_type",
                                 RegistrationType = "registration_type",
                                 Audio = "audio",
                                 AutoRecording = "auto_recording",
                                 EnforceLogin = "enforce_login",
                                 EnforceLoginDomains = "enforce_login_domains",
                                 AlternativeHosts = "alternative_hosts",
                                 CloseRegistration = "close_registration",
                                 ShowShareButton = "show_share_button",
                                 AllowMultipleDevices = "allow_multiple_devices",
                                 OnDemand = "on_demand",
                                 GlobalDialInCountries = "global_dial_in_countries",
                                 ContactName = "contact_name",
                                 ContactEmail = "contact_email",
                                 RegistrantsConfirmationEmail = "registrants_confirmation_email",
                                 RegistrantsRestrictNumber = "registrants_restrict_number",
                                 NotifyRegistrantgs = "notify_registrants",
                                 PostWebinarSurvey = "post_webinar_survey",
                                 SurveyUrl = "survey_url",
                                 RegistrantsEmailNotification = "registrants_email_notification",
                                 MeetingAuthentication = "meeting_authentication",
                                 AuthenticationOption = "authentication_option",
                                 AuthenticationDomains = "authentication_domains",
                                 AuthenticationName = "authentication_name"}

type webinar = {
     Uuid : option string,
     Id : option int,
     HostId : option string,
     Topic : string,
     Typ : webinar_type,
     StartTime : option time,
     Duration : option int,
     Timezone : option string,
     Password : option string,
     Agenda : option string,
     CreatedAt : option time,
     StartUrl : option string,
     JoinUrl : option string,
     RegistrationUrl : option string,
     Recurrence : option recurrence,
     Settings : option webinar_settings
}
val _ : json webinar = json_record_withOptional {Topic = "topic",
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
                        StartUrl = "start_url",
                        JoinUrl = "join_url",
                        RegistrationUrl = "registration_url",
                        Recurrence = "recurrence",
                        Settings = "settings"}

datatype file_type =
         MP4
       | M4A
       | TIMELINE
       | TRANSCRIPT
       | CHAT
       | CC
       | NoTypeYet
val _ : json file_type = json_derived
                             (fn x =>
                                 case x of
                                     "MP4" => MP4
                                   | "M4A" => M4A
                                   | "TIMELINE" => TIMELINE
                                   | "TRANSCRIPT" => TRANSCRIPT
                                   | "CHAT" => CHAT
                                   | "CC" => CC
                                   | "" => NoTypeYet
                                   | _ => error <xml>Bad Zoom file type {[x]}</xml>)
                             (fn x =>
                                 case x of
                                     MP4 => "MP4"
                                   | M4A => "M4A"
                                   | TIMELINE => "TIMELINE"
                                   | TRANSCRIPT => "TRANSCRIPT"
                                   | CHAT => "CHAT"
                                   | CC => "CC"
                                   | NoTypeYet => "")

datatype recording_type =
         SharedScreenWithSpeakerViewCC
       | SharedScreenWithSpeakerView
       | SharedScreenWithGalleryView
       | SpeakerView
       | GalleryView
       | SharedScreen
       | AudioOnly
       | AudioTranscript
       | ChatFile
       | Timeline
val _ : json recording_type = json_derived
                                  (fn x =>
                                      case x of
                                          "shared_screen_with_speaker_view(CC)" => SharedScreenWithSpeakerViewCC
                                        | "shared_screen_with_speaker_view" => SharedScreenWithSpeakerView
                                        | "shared_screen_with_gallery_view" => SharedScreenWithGalleryView
                                        | "speaker_view" => SpeakerView
                                        | "gallery_view" => GalleryView
                                        | "shared_screen" => SharedScreen
                                        | "audio_only" => AudioOnly
                                        | "audio_transcript" => AudioTranscript
                                        | "chat_file" => ChatFile
                                        | "TIMELINE" => Timeline
                                        | _ => error <xml>Bad Zoom recording type {[x]}</xml>)
                                  (fn x =>
                                      case x of
                                          SharedScreenWithSpeakerViewCC => "shared_screen_with_speaker_view(CC)"
                                        | SharedScreenWithSpeakerView => "shared_screen_with_speaker_view"
                                        | SharedScreenWithGalleryView => "shared_screen_with_gallery_view"
                                        | SpeakerView => "speaker_view"
                                        | GalleryView => "gallery_view"
                                        | SharedScreen => "shared_screen"
                                        | AudioOnly => "audio_only"
                                        | AudioTranscript => "audio_transcript"
                                        | ChatFile => "chat_file"
                                        | Timeline => "TIMELINE")

datatype recording_status =
         Processing
       | Completed
val _ : json recording_status = json_derived
                                (fn x =>
                                    case x of
                                        "processing" => Processing
                                      | "completed" => Completed
                                      | _ => error <xml>Bad Zoom recording status {[x]}</xml>)
                                (fn x =>
                                    case x of
                                        Processing => "processing"
                                      | Completed => "completed")

type recording_file = {
     Id : option string,
     MeetingId : option string,
     RecordingStart : option time,
     RecordingEnd : option string,
     FileType : option file_type,
     FileSize : option int,
     PlayUrl : option string,
     DownloadUrl : option string,
     Status : option recording_status,
     DeletedTime : option time,
     RecordingType : option recording_type
}
val _ : json recording_file = json_record_withOptional {}
                              {Id = "id",
                               MeetingId = "meeting_id",
                               RecordingStart = "recording_start",
                               RecordingEnd = "recording_end",
                               FileType = "file_type",
                               FileSize = "file_size",
                               PlayUrl = "play_url",
                               DownloadUrl = "download_url",
                               Status = "status",
                               DeletedTime = "deleted_time",
                               RecordingType = "recording_type"}

type recording = {
     Uuid : option string,
     Id : option int,
     AccountId : option string,
     HostId : option string,
     Topic : string,
     StartTime : option time,
     Duration : option int,
     TotalSize : option int,
     ShareUrl : option string,
     RecordingFiles : option (list recording_file)
}
val _ : json recording = json_record_withOptional {Topic = "topic"}
                         {Uuid = "uuid",
                          Id = "id",
                          AccountId = "account_id",
                          HostId = "host_id",
                          StartTime = "start_time",
                          Duration = "duration",
                          TotalSize = "total_size",
                          ShareUrl = "share_url",
                          RecordingFiles = "recording_files"}

datatype registrant_status =
         Approved
       | Pending
       | Denied
val _ : json registrant_status = json_derived
                                     (fn x =>
                                         case x of
                                             "approved" => Approved
                                           | "pending" => Pending
                                           | "denied" => Denied
                                           | _ => error <xml>Bad Zoom registrant status {[x]}</xml>)
                                     (fn x =>
                                         case x of
                                             Approved => "approved"
                                           | Pending => "pending"
                                           | Denied => "denied")

type registrant = {
     Id : option string,
     Email : string,
     FirstName : string,
     LastName : option string,
     Address : option string,
     City : option string,
     Country : option string,
     Zip : option string,
     State : option string,
     Phone : option string,
     Industry : option string,
     Org : option string,
     JobTitle : option string,
     PurchasingTimeFrame : option string,
     RoleInPurchaseProcess : option string,
     NoOfEmployees : option string,
     Comments : option string,
     Status : option registrant_status,
     CreateTime : option time,
     JoinUrl : option string
}
val _ : json registrant = json_record_withOptional {Email = "email",
                                                    FirstName = "first_name"}
                          {Id = "id",
                           LastName = "last_name",
                           Address = "address",
                           City = "city",
                           Country = "country",
                           Zip = "zip",
                           State = "state",
                           Phone = "phone",
                           Industry = "industry",
                           Org = "org",
                           JobTitle = "job_title",
                           PurchasingTimeFrame = "purchasing_time_frame",
                           RoleInPurchaseProcess = "role_in_purchase_process",
                           NoOfEmployees = "no_of_employees",
                           Comments = "comments",
                           Status = "status",
                           CreateTime = "create_time",
                           JoinUrl = "join_url"}

type registrant_response = {
     RegistrantId : string
}
val _ : json registrant_response = json_record {RegistrantId = "registrant_id"}

type participant = {
     Id : option string,
     UserId : option string,
     UserName : option string,
     Device : option string,
     IpAddress : option string,
     Location : option string,
     NetworkType : option string,
     Microphone : option string,
     Speaker : option string,
     Camera : option string,
     DataCenter : option string,
     ConnectionType : option string,
     JoinTime : option time,
     LeaveTime : option time,
     ShareApplication : option bool,
     ShareDesktop : option bool,
     ShareWhiteboard : option bool,
     Recording : option bool,
     PcName : option string,
     Domain : option string,
     MacAddr : option string,
     HarddiskId : option string,
     Version : option string,
     InRoomParticipants : option int,
     LeaveReason : option string
}

val _ : json participant = json_record_withOptional {}
                          {Id = "id",
                           UserId = "user_id",
                           UserName = "user_name",
                           Device = "device",
                           IpAddress = "ip_address",
                           Location = "location",
                           NetworkType = "network_type",
                           Microphone = "microphone",
                           Speaker = "speaker",
                           Camera = "camera",
                           DataCenter = "data_center",
                           ConnectionType = "connection_type",
                           JoinTime = "join_time",
                           LeaveTime = "leave_time",
                           ShareApplication = "share_application",
                           ShareDesktop = "share_desktop",
                           ShareWhiteboard = "share_whiteboard",
                           Recording = "recording",
                           PcName = "pc_name",
                           Domain = "domain",
                           MacAddr = "mac_addr",
                           HarddiskId = "harddisk_id",
                           Version = "version",
                           InRoomParticipants = "in_room_participants",
                           LeaveReason = "leave_reason"}

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Zoom to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://api.zoom.us/v2/"

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("Zoom response: " ^ show v);
        return v

    fun api url =
        tok <- token;
        logged (WorldFfi.get (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False)

    fun apiOpt url =
        tok <- token;
        logged (WorldFfi.getOpt (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False)

    fun apiPost url body =
        tok <- token;
        logged (WorldFfi.post (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) (Some "application/json") body)

    fun apiPaged [t ::: Type] (_ : json t) (listLabel : string) (url : string) : transaction (list t) =
        let
            val j : json {NextPageToken : option string, Records : list t} =
                json_record_withOptional {Records = listLabel}
                                         {NextPageToken = "next_page_token"}

            fun retrieve toko acc =
                page <- api (case toko of None => url | Some tok => url ^ "?next_page_token=" ^ tok);
                page <- return (@fromJson j page);
                case page.NextPageToken of
                    None => return (case acc of [] => page.Records | _ => List.rev (List.revAppend page.Records acc))
                  | Some "" => return (case acc of [] => page.Records | _ => List.rev (List.revAppend page.Records acc))
                  | Some _ => retrieve page.NextPageToken (List.revAppend page.Records acc)
        in
            retrieve None []
        end

    fun apiPagedOpt [t ::: Type] (_ : json t) (listLabel : string) (url : string) : transaction (list t) =
        let
            val j : json {NextPageToken : option string, Records : list t} =
                json_record_withOptional {Records = listLabel}
                                         {NextPageToken = "next_page_token"}

            fun retrieve tok acc =
                page <- api (url ^ "?next_page_token=" ^ tok);
                page <- return (@fromJson j page);
                case page.NextPageToken of
                    None => return (List.rev (List.revAppend page.Records acc))
                  | Some "" => return (List.rev (List.revAppend page.Records acc))
                  | Some tok => retrieve tok (List.revAppend page.Records acc)
        in
            page <- apiOpt url;
            case page of
                None => return []
              | Some page =>
                page <- return (@fromJson j page);
                case page.NextPageToken of
                    None => return page.Records
                  | Some "" => return page.Records
                  | Some tok => retrieve tok (List.rev page.Records)
        end

    structure Meetings = struct
        val list =
            apiPaged "meetings" "users/me/meetings"

        fun create x =
            s <- apiPost "users/me/meetings" (toJson x);
            return (fromJson s)

        fun get x =
            so <- apiOpt ("meetings/" ^ show x);
            return (Option.mp fromJson so)

        fun participants x =
            apiPagedOpt "participants" ("metrics/meetings/" ^ Urls.urlencode x ^ "/participants")

        structure Registrants = struct
            fun list x =
                apiPaged "registrants" ("meetings/" ^ show x ^ "/registrants")

            fun add mid p =
                s <- apiPost ("meetings/" ^ show mid ^ "/registrants") (toJson p);
                return (fromJson s : registrant_response).RegistrantId
        end
    end

    structure Webinars = struct
        val list =
            apiPaged "webinars" "users/me/webinars"

        fun create x =
            s <- apiPost "users/me/webinars" (toJson x);
            return (fromJson s)

        fun get x =
            so <- apiOpt ("webinars/" ^ show x);
            return (Option.mp fromJson so)

        structure Registrants = struct
            fun list x =
                apiPaged "registrants" ("webinars/" ^ show x ^ "/registrants")

            fun absentees x =
                apiPaged "registrants" ("past_webinars/" ^ Urls.urlencode x ^ "/absentees")
        end
    end

    structure CloudRecordings = struct
        val list =
            apiPaged "meetings" "users/me/recordings"

        fun get x =
            so <- apiOpt ("meetings/" ^ show x ^ "/recordings");
            return (Option.mp fromJson so)
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

functor TwoLegged(M : sig
                      val api_key : string
                      val api_secret : string
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

functor ThreeLegged(M : sig
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

                        val authorize_url = bless "https://api.zoom.us/oauth/authorize"
                        val access_token_url = bless "https://api.zoom.us/oauth/token"

                        val withToken = withToken
                        val scope = Some (Scope.toString scopes)
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
                           return <xml><button value="Log out of Zoom"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into Zoom"
                                               onclick={fn _ => redirect (url authorize)}/></xml>}/>
        </xml>
end
