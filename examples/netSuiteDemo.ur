open NetSuite

(* For this demo, it's necessary to create netSuiteSecrets.ur,
 * defining [account_id], [consumer_key], [consumer_secret], [token_id], and [token_secret]. *)
structure N = NetSuite.Make(NetSuite.TwoLegged(NetSuiteSecrets))

structure C = N.Table(struct
                          val stable = readError "Contact"
                          con fields = [Firstname = string,
                                        Email = string]
                          val labels = {Firstname = "firstname",
                                        Email = "email"}

                          con relations = [Company = [CompanyTitle = string]]
                          val rlabels = {Company = ("company",
                                                    "entity",
                                                    {CompanyTitle = "entityTitle"})}
                          val rjsons = {Company = _}
                      end)

fun main () =
    ls <- C.query (select [[Firstname = _, Email = _]]
                   |> rselect [#Company] [[CompanyTitle = _]]);
    return <xml><body>
      <ol>
        {List.mapX (fn r => <xml><li>{[r.Firstname]} @ {[r.Email]} @ {[r.CompanyTitle]}</li></xml>) ls}
      </ol>
    </body></xml>
