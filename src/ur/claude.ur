open Json
open Urls

functor TwoLegged(M : sig
                        val api_key : string
                    end) = struct
    open M

    val token = return (Some api_key)
end

type model = string
val read_model = _
val show_model = _

type complete_arg = {
     Model : model,
     Prompt : string,
     MaxTokensToSample : int
}
val _ : json complete_arg = json_record
                                {Model = "model",
                                 Prompt = "prompt",
                                 MaxTokensToSample = "max_tokens_to_sample"}

type complete_result = {
     Completion : string
}
val _ : json complete_result = json_record {Completion = "completion"}

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    val token =
        tok <- M.token;
        case tok of
            Some tok => return tok
          | None => error <xml>How odd: no Claude token!</xml>

    val urlPrefix = "https://api.anthropic.com/v1/"

    fun api url body =
        tok <- token;
        WorldFfi.post (bless (urlPrefix ^ url))
                      (WorldFfi.addHeader
                           (WorldFfi.addHeader WorldFfi.emptyHeaders "anthropic-version" "2023-06-01")
                           "x-api-key" tok)
                      (Some "application/json") body

    fun complete arg =
        r <- api "complete" (toJson arg);
        r <- return (String.trim (fromJson r : complete_result).Completion);
        debug ("Claude: " ^ r);
        return r
end
