signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val api_token : string
                  end) : sig
    val token : transaction (option string)
end

functor TwoLeggedDyn(M : sig
                         val settings : transaction string
                     end) : sig
    val token : transaction (option string)
end

type workspace_id
val show_workspace_id : show workspace_id
val inj_workspace_id : sql_injectable_prim workspace_id
val workspace_id : int -> workspace_id

type workspace = {
     Id : option workspace_id,
     Nam : string,
}

type template_id
val show_template_id : show template_id
val eq_template_id : eq template_id
val inj_template_id : sql_injectable_prim template_id
val template_id : int -> template_id

type template = {
     Id : option template_id,
     Nam : string
}

type column_id
val show_column_id : show column_id
val eq_column_id : eq column_id

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

type column = {
     Id : option column_id,
     Description : option string,
     Hidden : option bool,
     Index : option int,
     Primary : option bool,
     Title : option string,
     Typ : option column_type
}

type cell_id
val show_cell_id : show cell_id

type cell = {
     ColumnId : option column_id,
     Value : option Json.prim
}

type sheet_id
val show_sheet_id : show sheet_id
val inj_sheet_id : sql_injectable_prim sheet_id
val sheet_id : int -> sheet_id

type row_id
val show_row_id : show row_id

type row = {
     Id : option row_id,
     SheetId : option sheet_id,
     Cells : option (list cell),
     Columns : option (list column),
     RowNumber : option int
}

datatype source_type = Report | Sheet | Sight | Template

type source = {
     Id : int,
     Typ : source_type
}

type sheet = {
     Id : option sheet_id,
     Nam : string,
     FromId : option template_id,
     Columns : option (list column),
     Rows : option (list row),
     Source : option source
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
        val delete : sheet_id -> transaction unit
    end

    structure Rows : sig
        val add : sheet_id -> list row -> transaction (list row)
    end
end
