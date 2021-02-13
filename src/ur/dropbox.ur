open Json

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) = struct
    val token = return (Some M.token)
end

type file_request_id = string
val file_request_id_eq = _
val file_request_id_show = _
val file_request_id_inj = _

datatype grace_period =
         OneDay
       | TwoDays
       | SevenDays
       | ThirtyDays
       | Always
val _ : json grace_period = json_derived
                            (fn x =>
                                case x of
                                    "one_day" => OneDay
                                  | "two_days" => TwoDays
                                  | "seven_days" => SevenDays
                                  | "thirty_days" => ThirtyDays
                                  | "always" => Always
                                  | _ => error <xml>Bad Dropbox grace period {[x]}</xml>)
                            (fn x =>
                                case x of
                                    OneDay => "one_day"
                                  | TwoDays => "two_days"
                                  | SevenDays => "seven_days"
                                  | ThirtyDays => "thirty_days"
                                  | Always => "always")

type file_request_deadline = {
     Deadline : time,
     AllowLateUploads : option grace_period
}
val _ : json file_request_deadline = json_record_withOptional
                                     {Deadline = "deadline"}
                                     {AllowLateUploads = "allow_late_uploads"}

type file_request_parameters = {
     Title : string,
     Destination : string,
     Deadline : option file_request_deadline,
     Open : option bool,
     Description : option string
}
val _ : json file_request_parameters = json_record_withOptional
                                       {Title = "title",
                                        Destination = "destination"}
                                       {Deadline = "deadline",
                                        Open = "open",
                                        Description = "description"}

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
val _ : json file_request = json_record_withOptional
                            {Id = "id",
                             Url = "url",
                             Title = "title",
                             Created = "created",
                             IsOpen = "is_open",
                             FileCount = "file_count"}
                            {Destination = "destination",
                             Deadline = "deadline",
                             Description = "description"}

type cursor = {
     Cursor : string
}
val _ : json cursor = json_record {Cursor = "cursor"}

type file_requests = {
     FileRequests : list file_request,
     Cursor : string,
     HasMore : bool
}
val _ : json file_requests = json_record {FileRequests = "file_requests",
                                          Cursor = "cursor",
                                          HasMore = "has_more"}

type limit = {
     Limit : int
}
val _ : json limit = json_record {Limit = "limit"}

type id = {
     Id : string
}
val _ : json id = json_record {Id = "id"}

type shared_link = {
     Url : string,
     Password : option string
}
val _ : json shared_link = json_record_withOptional {Url = "url"}
                                                    {Password = "password"}

type template_filter_base' = {
     Tag : string,
     FilterSome : list string
}
val _ : json template_filter_base' = json_record {Tag = ".tag",
                                                  FilterSome = "filter_some"}

datatype template_filter_base =
         FilterSome of list string
val _ : json template_filter_base = json_derived
                                    (fn x =>
                                        case x.Tag of
                                            "filter_some" => FilterSome x.FilterSome
                                          | _ => error <xml>Bad Dropbox filter {[x.Tag]}</xml>)
                                    (fn (FilterSome x) => {Tag = ".filter_some",
                                                           FilterSome = x})

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
val _ : json list_folder_parameters = json_record_withOptional
                                      {Path = "path"}
                                      {Recursive = "recursive",
                                       IncludeDeleted = "include_deleted",
                                       IncludeHasExplicitSharedMembers = "include_has_explicit_shared_members",
                                       IncludeMountedFolders = "include_mounted_folders",
                                       Limit = "limit",
                                       SharedLink = "shared_link",
                                       IncludePropertyGroups = "include_property_groups",
                                       IncludeNonDownloadableFiles = "include_non_downloadable_files"}

type metadata = {
     Nam : string,
     PathLower : option string,
     PathDisplay : option string
}
val _ : json metadata = json_record_withOptional
                            {Nam = "name"}
                            {PathLower = "path_lower",
                             PathDisplay = "path_display"}

type files = {
     Entries : list metadata,
     Cursor : string,
     HasMore : bool
}
val _ : json files = json_record {Entries = "entries",
                                  Cursor = "cursor",
                                  HasMore = "has_more"}

type temporary_link = {
     Metadata : metadata,
     Link : string
}
val _ : json temporary_link = json_record {Metadata = "metadata",
                                           Link = "link"}

type path = {
     Path : string
}
val _ : json path = json_record {Path = "path"}

datatype requested_visibility =
         VisPublic
       | VisTeamOnly
       | VisPassword
val _ : json requested_visibility = json_derived
                                    (fn x =>
                                        case x of
                                            "public" => VisPublic
                                          | "team_only" => VisTeamOnly
                                          | "password" => VisPassword
                                          | _ => error <xml>Bad Dropbox visibility {[x]}</xml>)
                                    (fn x =>
                                        case x of
                                            VisPublic => "public"
                                          | VisTeamOnly => "team_only"
                                          | VisPassword => "password")

datatype link_audience =
         AudPublic
       | AudTeam
       | AudNoOne
       | AudPassword
val _ : json link_audience = json_derived
                             (fn x =>
                                 case x of
                                     "public" => AudPublic
                                   | "team" => AudTeam
                                   | "no_one" => AudNoOne
                                   | "password" => AudPassword
                                   | _ => error <xml>Bad Dropbox audience {[x]}</xml>)
                             (fn x =>
                                 case x of
                                     AudPublic => "public"
                                   | AudTeam => "team"
                                   | AudNoOne => "no_one"
                                   | AudPassword => "password")

datatype requested_link_access_level =
         Viewer
       | Editor
       | Max
val _ : json requested_link_access_level = json_derived
                                           (fn x =>
                                               case x of
                                                   "viewer" => Viewer
                                                 | "editor" => Editor
                                                 | "max" => Max
                                                 | _ => error <xml>Bad Dropbox access level {[x]}</xml>)
                                           (fn x =>
                                               case x of
                                                   Viewer => "viewer"
                                                 | Editor => "editor"
                                                 | Max => "max")

type shared_link_settings = {
     RequestedVisibility : option requested_visibility,
     LinkPassword : option string,
     Expires : option time,
     Audience : option link_audience,
     Access : option requested_link_access_level
}
val _ : json shared_link_settings = json_record_withOptional {}
                                    {RequestedVisibility = "requested_visibility",
                                     LinkPassword = "link_password",
                                     Expires = "expires",
                                     Audience = "audience",
                                     Access = "access"}

type shared_link_parameters = {
     Path : string,
     Settings : shared_link_settings
}
val _ : json shared_link_parameters = json_record {Path = "path",
                                                   Settings = "settings"}

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Dropbox to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://api.dropboxapi.com/2/"

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("Dropbox response: " ^ show v);
        return v

    fun api url body =
        tok <- token;
        logged (WorldFfi.post (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) (Some "application/json") body)

    structure FileRequests = struct
        val list =
            let
                fun list' cursor =
                    r <- (case cursor of
                              None => api "file_requests/list_v2" (toJson {Limit = 1000})
                            | Some c => api "list/continue" (toJson {Cursor = c}));
                    r <- return (fromJson r : file_requests);
                    if r.HasMore then
                        rest <- list' (Some r.Cursor);
                        return (List.append r.FileRequests rest)
                    else
                        return r.FileRequests
            in
                list' None
            end

        fun get id =
            r <- api "file_requests/get" (toJson {Id = id});
            return (fromJson r)

        fun create p =
            r <- api "file_requests/create" (toJson p);
            return (fromJson r)
    end

    structure Files = struct
        fun listFolder p =
            let
                fun list' cursor =
                    r <- (case cursor of
                              None => api "files/list_folder" (toJson p)
                            | Some c => api "files/list_folder/continue" (toJson {Cursor = c}));
                    r <- return (fromJson r : files);
                    if r.HasMore then
                        rest <- list' (Some r.Cursor);
                        return (List.append r.Entries rest)
                    else
                        return r.Entries
            in
                list' None
            end

        fun getTemporaryLink p =
            r <- api "files/get_temporary_link" (toJson {Path = p});
            return (fromJson r)
    end

    structure Sharing = struct
        fun createSharedLinkWithSettings p =
            r <- api "sharing/create_shared_link_with_settings" (toJson p);
            return (fromJson r : shared_link).Url
    end
end
