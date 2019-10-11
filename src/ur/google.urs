signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

(* First service: merely verifying which Google user we are dealing with *)
functor Login(M : S) : sig
    val authorize : { ReturnTo : url } -> transaction page
    val whoami : transaction (option string)
    (* ^-- Warning: returns an opaque numeric ID, not an e-mail address!
     * Google's API docs encourage use of this ID to prepare for e-mail changes by users. *)
    val logout : transaction unit
end

type message_id
val show_message_id : show message_id
val eq_message_id : eq message_id

type thread_id
val show_thread_id : show thread_id
val eq_thread_id : eq thread_id

type message = {
     Id : message_id,
     ThreadId : thread_id
}

type label_id
val show_label_id : show label_id

type header = {
     Nam : string,
     Value : string
}

type payload_metadata = {
     MimeType : string,
     Headers : list header
}

type history_id
val show_history_id : show history_id
val ord_history_id : ord history_id
val inj_history_id : sql_injectable_prim history_id
                     
type message_metadata = {
     Id : message_id,
     ThreadId : thread_id,
     LabelIds : list label_id,
     Snippet : string,
     HistoryId : history_id,
     InternalDate : string,
     Payload : payload_metadata,
     SizeEstimate : int
}

functor Gmail(M : S) : sig
    val authorize : { ReturnTo : url } -> transaction page
    val logout : transaction unit
    val loggedIn : transaction bool
    val emailAddress : transaction string (* ...of the logged-in user *)
                 
    val messages : transaction (list message)
    val messageMetadata : message_id -> transaction message_metadata
    val history : history_id -> transaction (list message * option history_id (* use for next call *))

    val ofThread : thread_id -> transaction url (* user-specific URL to a thread on Gmail *)
end

type calendar_id
val show_calendar_id : show calendar_id
val eq_calendar_id : eq calendar_id
                       
type calendar = {
     Id : calendar_id,
     Summary : string,
     Description : option string
}
                       
functor Calendar(M : S) : sig
   val authorize : { ReturnTo : url } -> transaction page
   val logout : transaction unit

   val calendars : transaction (list calendar)
end
