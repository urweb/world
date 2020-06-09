signature S = sig
    val api_key : string
    val api_secret : string
end

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : S) : AUTH

datatype meeting_type =
         Instant
       | Scheduled
       | RecurringUnfixed
       | RecurringFixed

datatype recurrence_type =
         Daily
       | Weekly
       | Monthly

datatype monthly_week =
         Last
       | First
       | Second
       | Third
       | Fourth

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

datatype approval_type =
         Automatically
       | Manually
       | NoRegistrationRequired

datatype registration_type =
         Once
       | Each
       | OnceForSeveral

datatype audio =
         Both
       | Telephony
       | Voip

datatype auto_recording =
         Local
       | Cloud
       | NoRecording

datatype global_dial_in_type =
         Toll
       | Tollfree

type global_dial_in_number = {
     Country : string,
     CountryName : string,
     City : string,
     Number : string,
     Typ : global_dial_in_type
}

type global_dial_in_country = {
     CountryName : string
}

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

datatype meeting_status =
         Waiting
       | Started
       | Finished

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

datatype webinar_type =
         Webinar
       | WebinarRecurringUnfixed
       | WebinarRecurringFixed

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

datatype file_type =
         MP4
       | M4A
       | TIMELINE
       | TRANSCRIPT
       | CHAT
       | CC
       | NoTypeYet

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

datatype recording_status =
         Processing
       | Completed

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

datatype registrant_status =
         Approved
       | Pending
       | Denied

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

functor Make(M : AUTH) : sig
    structure Meetings : sig
        val list : transaction (list meeting)
        val create : meeting -> transaction meeting
        val get : int (* ID *) -> transaction (option meeting)
    end

    structure Webinars : sig
        val list : transaction (list webinar)
        val create : webinar -> transaction webinar
        val get : int (* ID *) -> transaction (option webinar)

        structure Registrants : sig
            val list : int (* Webinar ID *) -> transaction (list registrant)
            val absentees : string (* Webinar UUID *) -> transaction (list registrant)
        end
    end

    structure CloudRecordings : sig
        val list : transaction (list recording)
        val get : int (* meeting ID *) -> transaction (option recording)
    end
end
