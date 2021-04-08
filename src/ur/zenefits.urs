functor TwoLegged(M : sig
                        val api_token : string
                    end) : sig
    val token : transaction (option string)
end

(** * API records *)

type person = {
     Id : string,
     FirstName : option string,
     PreferredName : option string,
     LastName : option string,
     WorkEmail : option string,
     PersonalEmail : option string
}

type employment = {
     AnnualSalary : option string
}

(** * Now for the actual methods.... *)

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure People : sig
        val list : transaction (list person)
    end

    structure Employments : sig
        val ofPerson : string -> transaction (list employment)
    end
end
