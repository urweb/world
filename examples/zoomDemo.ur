(* For this demo, it's necessary to create zoomSecrets.urs,
 * defining [api_key] and [api_secret]. *)
structure Z = Zoom.Make(Zoom.TwoLegged(ZoomSecrets))
              
fun main () =
    rs <- Z.Meetings.list;
    return <xml><body>
      <ul>
        {List.mapX (fn r => <xml><li>{[r.Topic]} ({[r.StartTime]}, {[case r.Typ of
                                                                          Zoom.Scheduled => "scheduled"
                                                                        | _ => "other"]})</li></xml>) rs}
      </ul>

      <h3>Create Meeting</h3>
      
      <form>
        Topic: <textbox{#Topic}/><br/>
        Starts: <textbox{#StartTime}/><br/>
        <submit action={create}/>
      </form>
    </body></xml>

and create r =
    Monad.ignore (Z.Meetings.create ({Topic = r.Topic,
                                      Typ = Zoom.Scheduled}
                                         ++ Api.optionals {StartTime = readError r.StartTime,
                                                           Duration = 30,
                                                           Settings = Api.optionals {Audio = Zoom.Voip}}));
    redirect (url (main ()))
