functor TwoLegged(M : sig
                        val api_key : string
                    end) : sig
    val token : transaction (option string)
end

(** * API records *)

(* LLM models supported by Claude *)
type model
val read_model : read model
val show_model : show model

(** * Now for the actual methods.... *)

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    val complete : {Model : model,
                    Prompt : string,
                    MaxTokensToSample : int}
                   -> transaction string
end
