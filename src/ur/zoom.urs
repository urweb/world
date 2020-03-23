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

type room = {
     Id : string,
     Nam : string,
     ActivationCode : string,
     Status : string
}

functor Make(M : AUTH) : sig
    structure Meetings : sig
        val list : transaction (list meeting)
        val create : meeting -> transaction meeting
    end

    structure Rooms : sig
        val list : transaction (list room)
    end
end
