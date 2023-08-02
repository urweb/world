(* For this demo, it's necessary to create smartsheetSecrets.ur,
 * defining [api_token]. *)
structure Z = Smartsheet.Make(Smartsheet.TwoLegged(SmartsheetSecrets))

val main =
    ts <- Z.Templates.list;
    return <xml><body>
      <ul>
        {List.mapX (fn r => <xml><li>{[r.Nam]}</li></xml>) ts}
      </ul>
    </body></xml>
