signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val api_token : string
                  end) : sig
    val token : transaction (option string)
end

datatype template_type =
         Report
       | Sheet

datatype access_level =
         ADMIN
       | COMMENTER
       | EDITOR
       | EDITOR_SHARE
       | OWNER
       | VIEWER

datatype global_template_type =
         BLANK_SHEET
       | PROJECT_SHEET
       | TASK_LIST

type template_id

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

type sheet_id

type sheet = {
     Id : option sheet_id,
     Nam : string,
     FromId : option template_id
}

functor Make(M : AUTH) : sig
    structure Templates : sig
        val list : transaction (list template)
    end

    structure Sheets : sig
        val createInWorkspace : template_id -> sheet -> transaction sheet_id
    end
end
