(* For this demo, it's necessary to create slackSecrets.urs,
 * defining [token]. *)
structure S = Slack.Make(Slack.TwoLegged(SlackSecrets))

val main =
    chs <- S.Conversations.list;
    return <xml><body>
      <ul>
        {List.mapX (fn ch => <xml><li><a href={S.Conversations.url ch}>{[ch.Nam]}</a></li></xml>) chs}
      </ul>
    </body></xml>
