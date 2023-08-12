open Json

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val api_token : string
                  end) = struct
    val token = return (Some M.api_token)
end

functor TwoLeggedDyn(M : sig
                         val settings : transaction string
                     end) = struct
    val token =
        tok <- M.settings;
        return (Some tok)
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

type template_id = int
val show_template_id = _

type template = {
     Id : option template_id,
     Nam : string
}

val _ : json template = json_record_withOptional
			    {Nam = "name"}
                            {Id = "id"}

type templates = {
     Data : list template
}

val _ : json templates = json_record {Data = "data"}

type column_id = int
val show_column_id = _
val eq_column_id = _

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

type column = {
     Id : option column_id,
     Description : option string,
     Hidden : option bool,
     Index : option int,
     Primary : option bool,
     Title : option string,
     Typ : option column_type
}

val _ : json column = json_record_withOptional
                          {Title = "title"}
                          {Id = "id",
                           Description = "description",
                           Hidden = "hidden",
                           Index = "index",
                           Primary = "primary",
                           Typ = "type"}

type cell_id = int
val show_cell_id = _

type cell = {
     ColumnId : option column_id,
     Value : option Json.prim
}

val _ : json cell = json_record_withOptional
                        {}
                        {ColumnId = "columnId",
                         Value = "value"}

type sheet_id = int
val show_sheet_id = _
val inj_sheet_id = _

type row_id = int
val show_row_id = _

type row = {
     Id : option row_id,
     SheetId : option sheet_id,
     Cells : option (list cell),
     Columns : option (list column),
     RowNumber : option int
}

val _ : json row = json_record_withOptional
                       {}
                       {Id = "id",
                        SheetId = "sheetId",
                        Cells = "cells",
                        Columns = "columns",
                        RowNumber = "rowNumber"}

type sheet = {
     Id : option sheet_id,
     Nam : string,
     FromId : option template_id,
     Columns : option (list column),
     Rows : option (list row)
}

val _ : json sheet = json_record_withOptional
                         {Nam = "name"}
                         {Id = "id",
                          FromId = "fromId",
                          Columns = "columns",
                          Rows = "rows"}

type workspace_id = int
val show_workspace_id = _

type workspace = {
     Id : option workspace_id,
     Nam : string
}

val _ : json workspace = json_record_withOptional
			     {Nam = "name"}
			     {Id = "id"}

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

type add_response = {
     Result : list row
}

val _ : json add_response = json_record {Result = "result"}

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

    structure Rows = struct
        fun add sid rs =
            s <- apiPost ("sheets/" ^ show sid ^ "/rows") (toJson rs);
            return ((fromJson s : add_response).Result)
    end
end
