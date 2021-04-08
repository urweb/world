(* For this demo, it's necessary to create zenefitsScrets.ur,
 * defining [api_token]. *)
structure Z = Zenefits.Make(Zenefits.TwoLegged(ZenefitsSecrets))

fun main () =
    ps <- Z.People.list;
    return <xml><body>
      <ul>
        {List.mapX (fn p => <xml><li>{[p.FirstName]} {[p.LastName]} (<tt>{[p.WorkEmail]}</tt>)</li></xml>) ps}
      </ul>
    </body></xml>
