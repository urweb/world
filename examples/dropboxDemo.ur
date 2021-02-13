(* For this demo, it's necessary to create dropboxSecrets.ur,
 * defining [api_key] and [api_secret]. *)
structure D = Dropbox.Make(Dropbox.TwoLegged(DropboxSecrets))

fun create r =
    m <- D.FileRequests.create ({Title = r.Title,
                                 Destination = r.Destination,
                                 Open = True}
                                    ++ Api.optionals {});
    return <xml><body>ID: {[m.Id]}</body></xml>

val main = return <xml><body>
  <h3>Create Request</h3>

  <form>
    Title: <textbox{#Title}/><br/>
    Destination: <textbox{#Destination}/><br/>
    <submit action={create}/>
  </form>
</body></xml>
