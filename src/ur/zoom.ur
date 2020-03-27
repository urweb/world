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
     City : string,
     Number : string,
     Typ : global_dial_in_type
}
val _ : json global_dial_in_number = json_record {Country = "country",
                                                  CountryName = "country_name",
                                                  City = "city",
                                                  Number = "number",
                                                  Typ = "type"}

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

type meetings_response = {
     Meetings : list meeting
}
val _ : json meetings_response = json_record {Meetings = "meetings"}

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

type recordings_response = {
    Meetings : list recording
}
val _ : json recordings_response = json_record {Meetings = "meetings"}
                         
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

    fun apiOpt url =
        tok <- token;
        WorldFfi.getOpt (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False

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

        fun get x =
            so <- apiOpt ("meetings/" ^ show x);
            return (Option.mp fromJson so)
    end

    structure CloudRecordings = struct
        val list =
            s <- api "users/me/recordings";
            return (fromJson s : recordings_response).Meetings

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
