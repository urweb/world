(* For this demo, it's necessary to create dropboxSecrets.ur,
 * defining [api_key] and [api_secret]. *)
structure D = Dropbox.Make(Dropbox.TwoLegged(DropboxSecrets))

fun create r =
    m <- D.FileRequests.create ({Title = r.Title,
                                 Destination = r.Destination}
                                    ++ Api.optionals {});
    return <xml><body>ID: {[m.Id]}</body></xml>

fun req id =
    r <- D.FileRequests.get id;
    fs <- (case r.Destination of
               None => return []
             | Some d => D.Files.listFolder ({Path = d} ++ Api.optionals {}));
    return <xml><body>
      Title: {[r.Title]}<br/>
      URL: {[r.Url]}<br/>
      Created: {[r.Created]}<br/>
      Open: {[r.IsOpen]}<br/>
      FileCount: {[r.FileCount]}<br/>
      Destination: {[r.Destination]}<br/>
      Description: {[r.Description]}

      <h4>Files</h4>

      <ul>
        {List.mapX (fn f => <xml><li>{[f.Nam]} ({[f.PathDisplay]})</li></xml>) fs}
      </ul>
    </body></xml>

val main =
    rs <- D.FileRequests.list;
    return <xml><body>
      <h3>Requests</h3>

      <ul>
        {List.mapX (fn r => <xml><li><a link={req r.Id}>{[r.Title]}</a> <i>({[r.Created]}, {[r.FileCount]})</i> <a href={bless r.Url}>[Upload]</a></li></xml>) rs}
      </ul>

      <h3>Create Request</h3>

      <form>
        Title: <textbox{#Title}/><br/>
        Destination: <textbox{#Destination}/><br/>
        <submit action={create}/>
      </form>
    </body></xml>
