open Json

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val api_token : string
                  end) = struct
    val token = return (Some M.api_token)
end

datatype template_type =
         Report
       | Sheet

val _ : json template_type = json_derived
                                 (fn x =>
                                     case x of
                                         "report" => Report
                                       | "sheet" => Sheet
                                       | _ => error <xml>Bad Smartsheet template type {[x]}</xml>)
                                (fn x =>
                                    case x of
                                        Report => "report"
                                      | Sheet => "sheet")

datatype access_level =
         ADMIN
       | COMMENTER
       | EDITOR
       | EDITOR_SHARE
       | OWNER
       | VIEWER

val _ : json access_level = json_derived
                                (fn x => case x of
                                             "ADMIN" => ADMIN
                                           | "COMMENTER" => COMMENTER
                                           | "EDITOR" => EDITOR
                                           | "EDITOR_SHARE" => EDITOR_SHARE
                                           | "OWNER" => OWNER
                                           | "VIEWER" => VIEWER
                                           | _ => error <xml>Bad Smartsheet access level {[x]}</xml>)
                                (fn x =>
                                    case x of
                                        ADMIN => "ADMIN"
                                      | COMMENTER => "COMMENTER"
                                      | EDITOR => "EDITOR"
                                      | EDITOR_SHARE => "EDITOR_SHARE"
                                      | OWNER => "OWNER"
                                      | VIEWER => "VIEWER")

datatype global_template_type =
         BLANK_SHEET
       | PROJECT_SHEET
       | TASK_LIST

val _ : json global_template_type = json_derived
                                        (fn x => case x of
                                                     "BLANK_SHEET" => BLANK_SHEET
                                                   | "PROJECT_SHEET" => PROJECT_SHEET
                                                   | "TASK_LIST" => TASK_LIST
                                                   | _ => error <xml>Bad Smartsheet global template type {[x]}</xml>)
                                        (fn x =>
                                            case x of
                                                BLANK_SHEET => "BLANK_SHEET"
                                              | PROJECT_SHEET => "PROJECT_SHEET"
                                              | TASK_LIST => "TASK_LIST")

type template_id = int
val show_template_id = _

type template = {
     Id : option template_id,
     Typ : option template_type,
     AccessLevel : option access_level,
     Blank : option bool,
     Categories : option (list string),
     GlobalTemplate : option global_template_type,
     Image : option string,
     LargeImage : option string,
     Nam : string,
     Tags : option (list string)
}

val _ : json template = json_record_withOptional
			    {Nam = "name"}
                            {Id = "id",
                             Typ = "type",
                             AccessLevel = "accessLevel",
                             Blank = "blank",
                             Categories = "categories",
                             GlobalTemplate = "globalTemplate",
                             Image = "image",
                             LargeImage = "largeImage",
                             Tags = "tags"}

type templates = {
     Data : list template
}

val _ : json templates = json_record {Data = "data"}

type column_id = int
val show_column_id = _

datatype system_column_type =
         AUTO_NUMBER
       | CREATED_BY
       | CREATED_DATE
       | MODIFIED_BY
       | MODIFIED_DATE

val _ : json system_column_type = json_derived
                                      (fn x =>
                                          case x of
                                              "AUTO_NUMBER" => AUTO_NUMBER
                                            | "CREATED_BY" => CREATED_BY
                                            | "CREATED_DATE" => CREATED_DATE
                                            | "MODIFIED_BY" => MODIFIED_BY
                                            | "MODIFIED_DATE" => MODIFIED_DATE
                                            | _ => error <xml>Bad Smartsheet system column type {[x]}</xml>)
                                      (fn x =>
                                          case x of
                                              AUTO_NUMBER => "AUTO_NUMBER"
                                            | CREATED_BY => "CREATED_BY"
                                            | CREATED_DATE => "CREATED_DATE"
                                            | MODIFIED_BY => "MODIFIED_BY"
                                            | MODIFIED_DATE => "MODIFIED_DATE")

datatype column_tag =
         CALENDAR_END_DATE
       | CALENDAR_START_DATE
       | CARD_DONE
       | GANTT_ALLOCATION
       | GANTT_ASSIGNED_RESOURCE
       | GANTT_DISPLAY_LABEL
       | GANTT_DURATION
       | GANTT_END_DATE
       | GANTT_PERCENT_COMPLETE
       | GANTT_PREDECESSOR
       | GANTT_START_DATE
       | BASELINE_START_DATE
       | BASELINE_END_DATE
       | BASELINE_VARIANCE

val _ : json column_tag = json_derived
                          (fn x =>
                              case x of
                                  "CALENDAR_END_DATE" => CALENDAR_END_DATE
                                | "CALENDAR_START_DATE" => CALENDAR_START_DATE
                                | "CARD_DONE" => CARD_DONE
                                | "GANTT_ALLOCATION" => GANTT_ALLOCATION
                                | "GANTT_ASSIGNED_RESOURCES" => GANTT_ASSIGNED_RESOURCE
                                | "GANTT_DISPLAY_LABEL" => GANTT_DISPLAY_LABEL
                                | "GANTT_DURATION" => GANTT_DURATION
                                | "GANTT_END_DATE" => GANTT_END_DATE
                                | "GANTT_PERCENT_COMPLETE" => GANTT_PERCENT_COMPLETE
                                | "GANTT_PREDECESSOR" => GANTT_PREDECESSOR
                                | "GANTT_START_DATE" => GANTT_START_DATE
                                | "BASELINE_START_DATE" => BASELINE_START_DATE
                                | "BASELINE_END_DATE" => BASELINE_END_DATE
                                | "BASELINE_VARIANCE" => BASELINE_VARIANCE
                                | _ => error <xml>Bad Smartsheet column tag {[x]}</xml>)
                          (fn x =>
                              case x of
                                  CALENDAR_END_DATE => "CALENDAR_END_DATE"
                                | CALENDAR_START_DATE => "CALENDAR_START_DATE"
                                | CARD_DONE => "CARD_DONE"
                                | GANTT_ALLOCATION => "GANTT_ALLOCATION"
                                | GANTT_ASSIGNED_RESOURCE => "GANTT_ASSIGNED_RESOURCE"
                                | GANTT_DISPLAY_LABEL => "GANTT_DISPLAY_LABEL"
                                | GANTT_DURATION => "GANTT_DURATION"
                                | GANTT_END_DATE => "GANTT_END_DATE"
                                | GANTT_PERCENT_COMPLETE => "GANTT_PERCENT_COMPLETE"
                                | GANTT_PREDECESSOR => "GANTT_PREDECESSOR"
                                | GANTT_START_DATE => "GANTT_START_DATE"
                                | BASELINE_START_DATE => "BASELINE_START_DATE"
                                | BASELINE_END_DATE => "BASELINE_END_DATE"
                                | BASELINE_VARIANCE => "BASELINE_VARIANCE")

datatype column_type =
         ABSTRACT_DATETIME
       | CHECKBOX
       | CONTACT_LIST
       | DATE
       | DATETIME
       | DURATION
       | MULTI_CONTACT_LIST
       | MULTI_PICKLIST
       | PICKLIST
       | PREDECESSOR
       | TEXT_NUMBER

val _ : json column_type = json_derived
                           (fn x =>
                               case x of
                                   "ABSTRACT_DATETIME" => ABSTRACT_DATETIME
                                 | "CHECKBOX" => CHECKBOX
                                 | "CONTACT_LIST" => CONTACT_LIST
                                 | "DATE" => DATE
                                 | "DATETIME" => DATETIME
                                 | "DURATION" => DURATION
                                 | "MULTI_CONTACT_LIST" => MULTI_CONTACT_LIST
                                 | "MULTI_PICKLIST" => MULTI_PICKLIST
                                 | "PICKLIST" => PICKLIST
                                 | "PREDECESSOR" => PREDECESSOR
                                 | "TEXT_NUMBER" => TEXT_NUMBER
                                 | _ => error <xml>Bad Smartsheet column type {[x]}</xml>)
                           (fn x =>
                               case x of
                                   ABSTRACT_DATETIME => "ABSTRACT_DATETIME"
                                 | CHECKBOX => "CHECKBOX"
                                 | CONTACT_LIST => "CONTACT_LIST"
                                 | DATE => "DATE"
                                 | DATETIME => "DATETIME"
                                 | DURATION => "DURATION"
                                 | MULTI_CONTACT_LIST => "MULTI_CONTACT_LIST"
                                 | MULTI_PICKLIST => "MULTI_PICKLIST"
                                 | PICKLIST => "PICKLIST"
                                 | PREDECESSOR => "PREDECESSOR"
                                 | TEXT_NUMBER => "TEXT_NUMBER")

datatype column_version =
         Column0
       | Column1
       | Column2

val _ : json column_version = json_derived
                              (fn x =>
                                  case x of
                                      0 => Column0
                                    | 1 => Column1
                                    | 2 => Column2
                                    | _ => error <xml>Bad Smartsheet column version {[x]}</xml>)
                              (fn x =>
                                  case x of
                                      Column0 => 0
                                    | Column1 => 1
                                    | Column2 => 2)

type column = {
     Id : option column_id,
     Description : option string,
     Hidden : option bool,
     Index : option int,
     Locked : option bool,
     LockedForUser : option bool,
     Primary : option bool,
     Symbol : option string,
     SystemColumnType : option system_column_type,
     Tags : option (list column_tag),
     Title : option string,
     Typ : option column_type,
     Validation : option bool,
     Version : option column_version,
     Width : option int
}

val _ : json column = json_record_withOptional
                          {Title = "title"}
                          {Id = "id",
                           Description = "description",
                           Hidden = "hidden",
                           Index = "index",
                           Locked = "locked",
                           LockedForUser = "lockedForUser",
                           Primary = "primary",
                           Symbol = "symbol",
                           SystemColumnType = "systemColumnType",
                           Tags = "tags",
                           Typ = "type",
                           Validation = "validation",
                           Version = "version",
                           Width = "width"}

type sheet_id = int
val show_sheet_id = _

type user_id = int
val show_user_id = _

type sheet = {
     Id : option sheet_id,
     Nam : string,
     FromId : option template_id,
     OwnerId : option user_id,
     AccessLevel : option access_level,
     Columns : option (list column)
}

val _ : json sheet = json_record_withOptional
                         {Nam = "name"}
                         {Id = "id",
                          FromId = "fromId",
                          OwnerId = "ownerId",
                          AccessLevel = "accessLevel",
                          Columns = "columns"}

type workspace_id = int
val show_workspace_id = _

type workspace = {
     Id : option workspace_id,
     AccessLevel : option access_level,
     Favorite : option bool,
     Nam : string,
     Permalink : option string
}

val _ : json workspace = json_record_withOptional
			     {Nam = "name"}
			     {Id = "id",
			      AccessLevel = "accessLevel",
			      Favorite = "favorite",
			      Permalink = "permalink"}

type workspaces = {
     Data : list workspace
}

val _ : json workspaces = json_record {Data = "data"}

type result = {
     Id : sheet_id
}
val _ : json result = json_record {Id = "id"}

type response = {
     Result : result
}
val _ : json response = json_record {Result = "result"}

type sheets = {
     Data : list sheet
}

val _ : json sheets = json_record {Data = "data"}

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Smartsheet to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://api.smartsheet.com/2.0/"

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("Smartsheet response: " ^ show v);
        return v

    fun api url =
        tok <- token;
        debug ("Smartsheet GET: " ^ prefix ^ url);
        logged (WorldFfi.get (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)

    fun apiPost url body =
        tok <- token;
        debug ("Smartsheet POST: " ^ prefix ^ url);
        debug ("Smartsheet body: " ^ body);
        logged (WorldFfi.post (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/json") body)

    structure Workspaces = struct
        val list =
            s <- api "workspaces?includeAll=true";
            return ((fromJson s : workspaces).Data)
    end

    structure Templates = struct
        val list =
            s <- api "templates?includeAll=true";
            return ((fromJson s : templates).Data)
    end

    structure Sheets = struct
        val list =
            s <- api "sheets?includeAll=true";
            return ((fromJson s : sheets).Data)

        fun get id =
            s <- api ("sheets/" ^ show id);
            return (fromJson s)

        fun createInWorkspace wid sh =
            s <- apiPost ("workspaces/" ^ show wid ^ "/sheets") (toJson sh);
            return ((fromJson s : response).Result.Id)
    end
end
