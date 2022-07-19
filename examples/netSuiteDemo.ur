(* For this demo, it's necessary to create netSuiteSecrets.ur,
 * defining [account_id], [consumer_key], [consumer_secret], [token_id], and [token_secret]. *)
structure N = NetSuite.Make(NetSuite.TwoLegged(NetSuiteSecrets))

fun schema tname =
    OpenAPI.Schema.Rec s <- N.Metadata.schema tname;
    case s.Properties of
        None => error <xml>No properties!</xml>
      | Some ps =>
        return <xml><body><ol>
          {List.mapX (fn (name, OpenAPI.Schema.Rec sc) => <xml><li>{[name]} : {[sc.Typ]}</li></xml>) ps}
        </ol></body></xml>

fun main () =
    md <- N.Metadata.tables;
    return <xml><body>
      <ol>
        {List.mapX (fn name => <xml><li><a link={schema name}>{[name]}</a></li></xml>) md}
      </ol>
    </body></xml>
