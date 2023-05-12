functor TwoLegged(M : sig
                        val api_token : string
                    end) : sig
    val token : transaction (option string)
end

(** * API records *)

(* LLM models supported by ChatGPT *)
type model
val read_model : read model
val show_model : show model

datatype role = System | User | Assistant
val read_role : read role
val show_role : show role

(** * Now for the actual methods.... *)

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure Chat : sig
        val completions : {Model : model,
                           Messages : list {Role : role,
                                            Content : string}}
                          -> transaction string
    end
end
