(* For this demo, it's necessary to create serviceNowSecrets.ur,
 * defining [instance], [client_id], and [client_secret]. *)

structure A = ServiceNow.ThreeLegged(struct
					 open ServiceNowSecrets
					 val https = False
					 val onCompletion = return <xml>Done.</xml>
					 val scopes = ServiceNow.Scope.empty
				     end)
structure S = ServiceNow.Make(A)

fun table_ name =
    cs <- S.Tables.columns name;
    return <xml><body>
      <h1>Columns</h1>

      <table>
	<tr><th>Name</th> <th>Type</th></tr>
	{List.mapX (fn c => <xml>
	  <tr><td>{[c.Nam]}</td> <td>{[c.Typ]}</td></tr>
	</xml>) cs}
      </table>
    </body></xml>

val main =
    toko <- A.token;
    case toko of
        None => return <xml><body><a link={A.authorize}>Log into ServiceNow</a></body></xml>
      | Some _ =>
	is <- S.Incidents.list;
	ts <- S.Tables.list;
	return <xml><body>
	  <h1>Incidents</h1>

	  <table>
	    <tr><th>Description</th></tr>

	    {List.mapX (fn i => <xml>
	      <tr><td>{[i.Description]}</td></tr>
	    </xml>) is}
	  </table>

	  <h1>Tables</h1>

	  <table>
	    <tr><th>Name</th></tr>

	    {List.mapX (fn t => <xml>
	      <tr><td><a link={table_ t.Nam}>{[t.Nam]}</a></td></tr>
	    </xml>) ts}
	  </table>
	</body></xml>

val incidents =
    is <- S.Table.list {Description = "description",
			Severity = "severity"} "incident";
    return <xml><body>
      <table>
	<tr><th>Description</th> <th>Severity</th></tr>

	{List.mapX (fn {Description = d : option string,
			Severity = s : option int} => <xml>
	  <tr><td>{[d]}</td> <td>{[s]}</td></tr>
	</xml>) is}
      </table>
    </body></xml>
