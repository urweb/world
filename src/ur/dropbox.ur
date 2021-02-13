open Json

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) = struct
    val token = return (Some M.token)
end

type file_request_id = string
val file_request_id_eq = _
val file_request_id_show = _
val file_request_id_inj = _

datatype grace_period =
         OneDay
       | TwoDays
       | SevenDays
       | ThirtyDays
       | Always
val _ : json grace_period = json_derived
                            (fn x =>
                                case x of
                                    "one_day" => OneDay
                                  | "two_days" => TwoDays
                                  | "seven_days" => SevenDays
                                  | "thirty_days" => ThirtyDays
                                  | "always" => Always
                                  | _ => error <xml>Bad Dropbox grace period {[x]}</xml>)
                            (fn x =>
                                case x of
                                    OneDay => "one_day"
                                  | TwoDays => "two_days"
                                  | SevenDays => "seven_days"
                                  | ThirtyDays => "thirty_days"
                                  | Always => "always")

type file_request_deadline = {
     Deadline : time,
     AllowLateUploads : option grace_period
}
val _ : json file_request_deadline = json_record_withOptional
                                     {Deadline = "deadline"}
                                     {AllowLateUploads = "allow_late_uploads"}

type file_request_parameters = {
     Title : string,
     Destination : string,
     Deadline : option file_request_deadline,
     Open : bool,
     Description : option string
}
val _ : json file_request_parameters = json_record_withOptional
                                       {Title = "title",
                                        Destination = "destination",
                                        Open = "open"}
                                       {Deadline = "deadline",
                                        Description = "description"}

type file_request = {
     Id : file_request_id,
     Url : string,
     Title : string,
     Created : time,
     IsOpen : bool,
     FileCount : int,
     Destination : option string,
     Deadline : option file_request_deadline,
     Description : option string
}
val _ : json file_request = json_record_withOptional
                            {Id = "id",
                             Url = "url",
                             Title = "title",
                             Created = "created",
                             IsOpen = "is_open",
                             FileCount = "file_count"}
                            {Destination = "destination",
                             Deadline = "deadline",
                             Description = "description"}

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Dropbox to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://api.dropboxapi.com/2/"

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("Dropbox response: " ^ show v);
        return v

    fun api url =
        tok <- token;
        logged (WorldFfi.get (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False)

    fun apiOpt url =
        tok <- token;
        logged (WorldFfi.getOpt (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False)

    fun apiPost url body =
        tok <- token;
        logged (WorldFfi.post (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) (Some "application/json") body)

    structure FileRequests = struct
        fun create p =
            r <- apiPost "file_requests/create" (toJson p);
            return (fromJson r)
    end
end
