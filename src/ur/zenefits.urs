functor TwoLegged(M : sig
                        val api_token : string
                    end) : sig
    val token : transaction (option string)
end

(** * Person API records *)

type person = {
     FirstName : option string,
     PreferredName : option string,
     LastName : option string,
     WorkEmail : option string,
     PersonalEmail : option string
}

(** * Now for the actual methods.... *)

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure People : sig
        val list : transaction (list person)
    end
end
