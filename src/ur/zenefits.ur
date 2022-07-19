open Json
open Urls

functor TwoLegged(M : sig
                        val api_token : string
                    end) = struct
    open M

    val token = return (Some api_token)
end

type person = {
     Id : string,
     FirstName : option string,
     PreferredName : option string,
     LastName : option string,
     WorkEmail : option string,
     PersonalEmail : option string
}
val _ : json person = json_record
                          {Id = "id",
                           FirstName = "first_name",
                           PreferredName = "preferred_name",
                           LastName = "last_name",
                           WorkEmail = "work_email",
                           PersonalEmail = "personal_email"}

type employment = {
     AnnualSalary : option string
}
val _ : json employment = json_record
                          {AnnualSalary = "annual_salary"}

type subresults a = {
     Data : list a,
     NextUrl : option string
}
fun json_subresults [a] (_ : json a) : json (subresults a) =
    json_record {Data = "data", NextUrl = "next_url"}

type results a = {
     Data : subresults a
}
fun json_results [a] (_ : json a) : json (results a) =
    json_record {Data = "data"}

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    val token =
        tok <- M.token;
        case tok of
            Some tok => return tok
          | None => error <xml>How odd: no Zenefits token!</xml>

    val urlPrefix = "https://api.zenefits.com/"

    fun api url =
        tok <- token;
        WorldFfi.get url (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) True

    fun apiPaged' [a] (_ : json a) (url : url) : transaction (list a) =
        s <- api url;
        debug (show url ^ " -> " ^ s);
        r <- return (fromJson s : results a);
        case r.Data.NextUrl of
            None => return r.Data.Data
          | Some nu =>
            r' <- apiPaged' (bless nu);
            return (List.append r.Data.Data r')

    fun apiPaged [a] (_ : json a) (url : string) : transaction (list a) =
        apiPaged' (bless (urlPrefix ^ url))

    structure People = struct
        val list = apiPaged "core/people"
    end

    structure Employments = struct
        fun ofPerson p = apiPaged ("core/people/" ^ urlencode p ^ "/employments")
    end
end
