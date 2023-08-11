(* For this demo, it's necessary to create smartsheetSecrets.ur,
 * defining [api_token]. *)
structure S = Smartsheet.Make(Smartsheet.TwoLegged(SmartsheetSecrets))

fun create tid wid r =
    sid <- S.Sheets.createInWorkspace wid ({Nam = r.Nam} ++ Api.optionals {FromId = tid});
    return <xml><body>
      I created sheet #{[sid]}.
    </body></xml>

fun workspace tid wid =
    return <xml><body>
      <form>
	New sheet name: <textbox{#Nam}/><br/>
	<submit action={create tid wid}/>
      </form>
    </body></xml>

fun template tid =
    ws <- S.Workspaces.list;
    return <xml><body>
      <h2>Workspaces</h2>
      <ul>
        {List.mapX (fn r =>
		       case r.Id of
			   None => error <xml>No ID returned for workspace</xml>
			 | Some wid => <xml><li><a link={workspace tid wid}>{[r.Nam]}</a></li></xml>) ws}
      </ul>
    </body></xml>

datatype input_widget = StringWidget of source string | BoolWidget of source bool

fun addRow sid cs =
    Monad.ignore (S.Rows.add sid (Api.optionals {Cells = cs} :: []))

fun sheet sid =
    sh <- S.Sheets.get sid;
    ws <- List.mapM (fn c =>
                        case (c.Id, c.Title) of
                            (Some cid, Some ttl) =>
                            (case c.Typ of
                                 Some Smartsheet.CHECKBOX =>
                                 s <- source False;
                                 return (cid, ttl, BoolWidget s)
                               | _ =>
                                 s <- source "";
                                 return (cid, ttl, StringWidget s))
                          | _ => error <xml>Missing column info</xml>) (Option.get [] sh.Columns);
    return <xml><body>
      <h2>Columns</h2>
      <table>
        <tr> <th>Name</th> <th>Type</th> </tr>
        {List.mapX (fn c => <xml><tr>
          <td>{[c.Title]}</td>
          <td>{[case c.Typ of
                    Some Smartsheet.TEXT_NUMBER => "default"
                  | Some Smartsheet.CHECKBOX => "bool"
                  | _ => "other"]}</td>
        </tr></xml>) (Option.get [] sh.Columns)}
      </table>

      <h2>Rows</h2>
      <table>
        {List.mapX (fn r => <xml><tr>
          {List.mapX (fn c => <xml><td>{[c.Value]}</td></xml>) (Option.get [] r.Cells)}
        </tr></xml>) (Option.get [] sh.Rows)}
      </table>

      <h2>New row</h2>

      <table>
        {List.mapX (fn (_, ttl, w) => <xml><tr>
          <th>{[ttl]}</th>
          <td>{case w of
                   StringWidget s => <xml><ctextbox source={s}/></xml>
                 | BoolWidget s => <xml><ccheckbox source={s}/></xml>}</td>
        </tr></xml>) ws}
      </table>

      <button value="Create"
              onclick={fn _ =>
                          cs <- List.mapM (fn (cid, _, w) =>
                                              v <- (case w of
                                                        StringWidget s =>
                                                        v <- get s;
                                                        return (Json.String v)
                                                      | BoolWidget s =>
                                                        v <- get s;
                                                        return (Json.Bool v));
                                              return (Api.optionals {ColumnId = cid,
                                                                     Value = v})) ws;
                          rpc (addRow sid cs)}/>
    </body></xml>

val main =
    shs <- S.Sheets.list;
    ts <- S.Templates.list;
    return <xml><body>
      <h2>Sheets</h2>
      <ul>
        {List.mapX (fn r =>
		       case r.Id of
			   None => error <xml>No ID returned for sheet</xml>
			 | Some sid => <xml><li><a link={sheet sid}>{[r.Nam]}</a></li></xml>) shs}
      </ul>

      <h2>Templates</h2>
      <ul>
        {List.mapX (fn r =>
		       case r.Id of
			   None => error <xml>No ID returned for template</xml>
			 | Some tid => <xml><li><a link={template tid}>{[r.Nam]}</a></li></xml>) ts}
      </ul>
    </body></xml>
