signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) : sig
    val token : transaction (option string)
end

type file_request_id
val file_request_id_eq : eq file_request_id
val file_request_id_show : show file_request_id
val file_request_id_inj : sql_injectable_prim file_request_id

datatype grace_period =
         OneDay
       | TwoDays
       | SevenDays
       | ThirtyDays
       | Always

type file_request_deadline = {
     Deadline : time,
     AllowLateUploads : option grace_period
}

type file_request_parameters = {
     Title : string,
     Destination : string,
     Deadline : option file_request_deadline,
     Open : option bool,
     Description : option string
}

type file_request = {
     Id : file_request_id,
     Url : string,
     Title : string,
     Created : time,
     IsOpen : bool,
     FileCount : int,
     Destination : option string,
     Deadline : option file_request_deadline,
     Description : option string
}

type shared_link = {
     Url : string,
     Password : option string
}

datatype template_filter_base =
         FilterSome of list string

type list_folder_parameters = {
     Path : string,
     Recursive : option bool,
     IncludeDeleted : option bool,
     IncludeHasExplicitSharedMembers : option bool,
     IncludeMountedFolders : option bool,
     Limit : option int,
     SharedLink : option shared_link,
     IncludePropertyGroups : option template_filter_base,
     IncludeNonDownloadableFiles : option bool
}

type metadata = {
     Nam : string,
     PathLower : option string,
     PathDisplay : option string
}

type temporary_link = {
     Metadata : metadata,
     Link : string
}

datatype requested_visibility =
         VisPublic
       | VisTeamOnly
       | VisPassword

datatype link_audience =
         AudPublic
       | AudTeam
       | AudNoOne
       | AudPassword

datatype requested_link_access_level =
         Viewer
       | Editor
       | Max

type shared_link_settings = {
     RequestedVisibility : option requested_visibility,
     LinkPassword : option string,
     Expires : option time,
     Audience : option link_audience,
     Access : option requested_link_access_level
}

type shared_link_parameters = {
     Path : string,
     Settings : shared_link_settings
}

functor Make(M : AUTH) : sig
    structure FileRequests : sig
        val list : transaction (list file_request)
        val get : file_request_id -> transaction file_request
        val create : file_request_parameters -> transaction file_request
    end

    structure Files : sig
        val listFolder : list_folder_parameters -> transaction (list metadata)
        val getTemporaryLink : string (* path *) -> transaction temporary_link
    end

    structure Sharing : sig
        val createSharedLinkWithSettings : shared_link_parameters -> transaction string (* url *)
    end
end
