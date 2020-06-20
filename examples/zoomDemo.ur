(* For this demo, it's necessary to create zoomSecrets.urs,
 * defining [api_key] and [api_secret]. *)
(*structure Z = Zoom.Make(Zoom.TwoLegged(ZoomSecrets))*)

(* Here's an alternative authentication flow. *)
structure A = Zoom.ThreeLegged(struct
                                   open ZoomSecrets
                                   val https = False
                                   val onCompletion = return <xml>Done.</xml>
                                   val scopes = Zoom.Scope.meetingWrite
                               end)
structure Z = Zoom.Make(A)

fun meeting m =
    case m.Id of
        None => error <xml>Meeting has no ID!</xml>
      | Some id =>
        (ro : option Zoom.recording) <- Z.CloudRecordings.get id;
        case (m.StartUrl, m.JoinUrl) of
            (Some start, Some join) =>
            return <xml><body>
              <h3>Meeting ID {[id]}</h3>

              <h4><a href={bless start}>Start</a>, <a href={bless join}>Join</a></h4>

              <ul>
                {case (r <- ro; r.ShareUrl) of
                     None => <xml></xml>
                   | Some u => <xml><li><a href={bless u}>[Share]</a></li></xml>}
                {case (r <- ro; r.RecordingFiles) of
                     None => <xml></xml>
                   | Some rfs =>
                     List.mapX (fn r => <xml><li>Recording of size {[r.FileSize]}
                       {case r.PlayUrl of
                            None => <xml></xml>
                          | Some u => <xml><a href={bless u}>[Play]</a></xml>}
                       {case r.DownloadUrl of
                            None => <xml></xml>
                          | Some u => <xml><a href={bless u}>[Download]</a></xml>}</li></xml>) rfs}
              </ul>
            </body></xml>
          | _ => error <xml>New meeting is missing URL to start or join.</xml>

fun create r =
    m <- Z.Meetings.create ({Topic = r.Topic,
                             Typ = Zoom.Scheduled}
                                ++ Api.optionals {StartTime = readError r.StartTime,
                                                  Duration = 30,
                                                  Settings = Api.optionals {HostVideo = True,
                                                                            AutoRecording = Zoom.Cloud}});
    meeting m

fun lookup id =
    mo <- Z.Meetings.get id;
    case mo of
        None => error <xml>Missing meeting</xml>
      | Some m => meeting m

val main =
    toko <- A.token;
    case toko of
        None => return <xml><body><a link={A.authorize}>Log into Zoom</a></body></xml>
      | Some _ =>
        rs <- Z.Meetings.list;
        return <xml><body>
          <ul>
            {List.mapX (fn r => <xml><li><a link={lookup (Option.get 0 r.Id)}>{[r.Topic]}</a> ({[r.StartTime]})</li></xml>) rs}
          </ul>

          <h3>Create Meeting</h3>

          <form>
            Topic: <textbox{#Topic}/><br/>
            Starts: <textbox{#StartTime}/><br/>
            <submit action={create}/>
          </form>
        </body></xml>
