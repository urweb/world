signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) : sig
    val token : transaction (option string)
end

type file_request_id
val file_request_id_eq : eq file_request_id
val file_request_id_show : show file_request_id
val file_request_id_inj : sql_injectable_prim file_request_id

datatype grace_period =
         OneDay
       | TwoDays
       | SevenDays
       | ThirtyDays
       | Always

type file_request_deadline = {
     Deadline : time,
     AllowLateUploads : option grace_period
}

type file_request_parameters = {
     Title : string,
     Destination : string,
     Deadline : option file_request_deadline,
     Open : bool,
     Description : option string
}

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

functor Make(M : AUTH) : sig
    structure FileRequests : sig
        val create : file_request_parameters -> transaction file_request
    end
end
