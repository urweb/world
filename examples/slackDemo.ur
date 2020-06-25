(* For this demo, it's necessary to create slackSecrets.urs,
 * defining [token]. *)
structure S = Slack.Make(Slack.TwoLegged(SlackSecrets))

val main =
    chs <- S.Conversations.list;
    return <xml><body>
      <ul>
        {List.mapX (fn ch => <xml><li>{[ch.Nam]}</li></xml>) chs}
      </ul>
    </body></xml>
