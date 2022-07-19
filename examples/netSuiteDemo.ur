(* For this demo, it's necessary to create netSuiteSecrets.ur,
 * defining [account_id], [consumer_key], [consumer_secret], [token_id], and [token_secret]. *)
structure N = NetSuite.Make(NetSuite.TwoLegged(NetSuiteSecrets))

fun main () =
    md <- N.metadata;
    return <xml><body>
      <pre>{[md]}</pre>
    </body></xml>
