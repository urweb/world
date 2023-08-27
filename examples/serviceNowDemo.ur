(* For this demo, it's necessary to create serviceNowSecrets.ur,
 * defining [instance], [client_id], and [client_secret]. *)

structure A = ServiceNow.ThreeLegged(struct
					 open ServiceNowSecrets
					 val https = False
					 val onCompletion = return <xml>Done.</xml>
					 val scopes = ServiceNow.Scope.empty
				     end)
structure S = ServiceNow.Make(A)

val main =
    toko <- A.token;
    case toko of
        None => return <xml><body><a link={A.authorize}>Log into ServiceNow</a></body></xml>
      | Some _ =>
	is <- S.Incidents.list;
	return <xml><body>
	  <h1>Incidents</h1>

	  <table>
	    <tr><th>Description</th></tr>

	    {List.mapX (fn i => <xml>
	      <tr><td>{[i.Description]}</td></tr>
	    </xml>) is}
	  </table>
	</body></xml>
