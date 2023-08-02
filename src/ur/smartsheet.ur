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

type template = {
     Id : option int,
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

    structure Templates = struct
        val list =
            s <- api "templates?includeAll=true";
            return ((fromJson s : templates).Data)
    end
end
