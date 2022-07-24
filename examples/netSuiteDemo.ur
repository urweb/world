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

structure C' = N.Table(struct
                           val stable = readError "Contact"
                           con fields = [Firstname = string,
                                         Email = string,
                                         Company = string]
                           val labels = {Firstname = "firstname",
                                         Email = "email",
                                         Company = "company"}

                           con relations = []
                           val rlabels = {}
                           val rjsons = {}
                       end)

fun create r =
    C'.insert (NetSuite.values r);
    return <xml><body>OK.</body></xml>

fun update id r =
    C'.update id (NetSuite.values r);
    return <xml><body>OK.</body></xml>

fun edit id =
    ls <- C'.query (select [[Firstname = _, Email = _, Company = _]]
                    |> wher (eq (field [#Id]) (string id)));
    case ls of
        r :: [] => return <xml><body>
          <form>
            First name: <textbox{#Firstname} value={Option.get "" r.Firstname}/><br/>
            Email: <textbox{#Email} value={Option.get "" r.Email}/><br/>
            Company: <textbox{#Company} value={Option.get "" r.Company}/><br/>
            <submit action={update id}/>
          </form>
        </body></xml>
      | _ => error <xml>Bad query response</xml>

fun main () =
    ls <- C.query (select [[Id = _, Firstname = _, Email = _]]
                   |> rselect [#Company] [[CompanyTitle = _]]);
    return <xml><body>
      <ol>
        {List.mapX (fn r => <xml><li><a link={edit (Option.get "" r.Id)}>{[r.Firstname]} @ {[r.Email]} @ {[r.CompanyTitle]}</a></li></xml>) ls}
      </ol>

      <form>
        First name: <textbox{#Firstname}/><br/>
        Email: <textbox{#Email}/><br/>
        Company: <textbox{#Company}/><br/>
        <submit action={create}/>
      </form>
    </body></xml>
