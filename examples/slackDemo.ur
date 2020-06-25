(* For this demo, it's necessary to create slackSecrets.urs,
 * defining [token]. *)
structure S = Slack.Make(Slack.TwoLegged(SlackSecrets))

fun postMessage ch text =
    Monad.ignore (S.Chat.postMessage {Channel = ch, Text = text})

fun channel ch =
    text <- source "";
    ms <- S.Conversations.history ch;
    return <xml><body>
      <ol>
        {List.mapX (fn m => <xml><li>{[m.User]}: {[m.Text]}</li></xml>) ms}
      </ol>
      <hr/>
      Post message: <ctextbox source={text}/> <button value="Go" onclick={fn _ => text <- get text; rpc (postMessage ch text); redirect (url (channel ch))}/>
    </body></xml>

fun newChannel name =
    Monad.ignore (S.Conversations.create name)

fun main () =
    name <- source "";
    chs <- S.Conversations.list;
    return <xml><body>
      <ul>
        {List.mapX (fn ch => <xml><li><a href={S.Conversations.url ch}>{[ch.Nam]}</a> <a link={channel ch.Id}>[more]</a></li></xml>) chs}
      </ul>

      <hr/>

      Create channel: <ctextbox source={name}/> <button value="Go" onclick={fn _ => name <- get name; rpc (newChannel name); redirect (url (main ()))}/>
    </body></xml>
