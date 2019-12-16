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

type account
val read_account : read account
val show_account : show account
                           
functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure Accounts : sig
        val list : instance -> transaction (list account)
    end
end
