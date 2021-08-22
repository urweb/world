(* For this demo, it's necessary to create zenefitsSecrets.ur,
 * defining [api_token]. *)
structure Z = Zenefits.Make(Zenefits.TwoLegged(ZenefitsSecrets))

fun main () =
    ps <- Z.People.list;
    ps <- List.mapM (fn p =>
                        emps <- Z.Employments.ofPerson p.Id;
                        sal <- return (case emps of
                                           [] => 0.0
                                         | emp :: _ => Option.get 0.0 (Option.bind read emp.AnnualSalary));
                        return (p, sal)) ps;
    return <xml><body>
      <ul>
        {List.mapX (fn (p, sal) => <xml><li>{[p.FirstName]} {[p.LastName]} (<tt>{[p.WorkEmail]}</tt>) -- {[sal]}</li></xml>) ps}
      </ul>
    </body></xml>
