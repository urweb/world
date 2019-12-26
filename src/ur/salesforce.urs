type instance
val read_instance : read instance
val show_instance : show instance

functor ThreeLegged(M : sig
                        val client_id : string
                        val client_secret : string

                        val https : bool
                        val sandbox : bool
                    end) : sig
    val token : transaction (option string)
    val status : transaction xbody
end

type account_name
val read_account_name : read account_name
val show_account_name : show account_name

type account_id
val read_account_id : read account_id
val show_account_id : show account_id

type account = {
     Id : account_id,
     Nam : account_name,
     Website : option string
}

type new_account = {
     Nam : account_name,
     Website : option string
}

type new_contact = {
     FirstName : string,
     LastName : string,
     Account : option account_id,
     Email : option string
}

type contact_name = {
     FirstName : string,
     LastName : string
}

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure Accounts : sig
        val list : instance -> transaction (list account)
        val existsByName : instance -> account_name -> transaction bool
        val lookupByName : instance -> account_name -> transaction (option account)
        val insert : instance -> new_account -> transaction unit
    end

    structure Contacts : sig
        val existsByName : instance -> contact_name -> transaction bool
        val insert : instance -> new_contact -> transaction unit
    end
end
