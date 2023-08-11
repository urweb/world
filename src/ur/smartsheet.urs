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

type workspace_id
val show_workspace_id : show workspace_id

type workspace = {
     Id : option workspace_id,
     AccessLevel : option access_level,
     Favorite : option bool,
     Nam : string,
     Permalink : option string
}

type template_id
val show_template_id : show template_id

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

type column_id
val show_column_id : show column_id

datatype system_column_type =
         AUTO_NUMBER
       | CREATED_BY
       | CREATED_DATE
       | MODIFIED_BY
       | MODIFIED_DATE

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

datatype column_version =
         Column0
       | Column1
       | Column2

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

type cell_id
val show_cell_id : show cell_id

type cell = {
     ColumnId : option column_id,
     ColumnType : option string,
     ConditionalFormat : option string,
     DisplayValue : option string,
     Format : option string,
     Formula : option string,
     Strict : option string,
     Value : option Json.prim
}

type sheet_id
val show_sheet_id : show sheet_id

type user_id
val show_user_id : show user_id

type row_id
val show_row_id : show row_id

type row = {
     Id : option row_id,
     SheetId : option sheet_id,
     AccessLevel : option access_level,
     Cells : option (list cell),
     Columns : option (list column),
     ConditionalFormat : option string,
     Expanded : option bool,
     FilteredOut : option bool,
     Format : option string,
     InCriticalPath : option bool,
     Locked : option bool,
     LockedForUser : option bool,
     RowNumber : option int,
     Version : option int
}

type sheet = {
     Id : option sheet_id,
     Nam : string,
     FromId : option template_id,
     OwnerId : option user_id,
     AccessLevel : option access_level,
     Columns : option (list column),
     Rows : option (list row)
}

functor Make(M : AUTH) : sig
    structure Workspaces : sig
        val list : transaction (list workspace)
    end

    structure Templates : sig
        val list : transaction (list template)
    end

    structure Sheets : sig
        val list : transaction (list sheet)
        val get : sheet_id -> transaction sheet
        val createInWorkspace : workspace_id -> sheet -> transaction sheet_id
    end

    structure Rows : sig
        val add : sheet_id -> list row -> transaction (list row)
    end
end
