(* Which permissions may we ask for, limiting what the app may do thereafter? *)
structure Scope : sig
    type t
    val empty : t
    val union : t -> t -> t
    val readonly : t -> bool

    val calendar : t
    val calendar_readonly : t
    val calendar_admin : t
    val gmail_readonly : t

    (* SUPER-COUNTERINTUITIVE FACT TO REMEMBER: to make domain-wide delegation
     * work properly (as is needed for two-legged authentication with
     * impersonation), the service account must be granted use of
     * [calendar_admin], not just whatever scopes you would use for normal
     * calendar access! *)
end

(* Two authentication methods are supported, each providing this signature. *)

signature AUTH = sig
    val readonly : bool
    val token : transaction (option string)
end

(* Two-legged is when the app is authorized to act directly, not on behalf of
 * any user. *)
functor TwoLegged(M : sig
                      val service_account : string
                      val private_key : string
                      val impersonated_user : string
                      val https : bool

                      val scopes : Scope.t
                  end) : AUTH

(* Three-legged is when the app always asks a user for delegated privileges,
 * via OAuth 2. *)
functor ThreeLegged(M : sig
                        val client_id : string
                        val client_secret : string
                        val https : bool

                        val scopes : Scope.t
                    end) : sig
    include AUTH

    val status : transaction xbody
    val authorize : { ReturnTo : url } -> transaction page
    val loggedIn : transaction bool
    val logout : transaction unit
end

(** * Gmail types *)

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

(** * Calendar types *)

type calendar_id
val show_calendar_id : show calendar_id
val read_calendar_id : read calendar_id
val eq_calendar_id : eq calendar_id
                       
type calendar = {
     Id : calendar_id,
     Summary : string,
     Description : option string
}

type event_id
val show_event_id : show event_id
val eq_event_id : eq event_id

datatype when =
         Time of time
       | Date of {Year : int, Month : int, Day : int}
val show_when : show when

datatype responseStatus =
         NeedsAction
       | Declined
       | Tentative
       | Accepted

type attendee = {
     AdditionalGuests : option int,
     Comment : option string,
     DisplayName : option string,
     Email : option string,
     Optional : option bool,
     Organizer : option bool,
     Resource : option bool,
     ResponseStatus : responseStatus,
     Self : option bool
}

type event = {
     Id : event_id,
     Summary : option string,
     Description : option string,
     Start : option when,
     End : option when,
     Attendees : option (list attendee)
}

type newEvent = {
     Summary : option string,
     Description : option string,
     Start : option when,
     End : option when,
     Attendees : option (list attendee)
}

datatype updatesMode =
         All
       | ExternalOnly
       | NoneUpdates


(** * The main API interface *)
                        
functor Make(M : AUTH) : sig
    val emailAddress : transaction (option string)

    structure Gmail : sig
        val messages : transaction (list message)
        val messageMetadata : message_id -> transaction message_metadata
        val history : history_id -> transaction (list message * option history_id (* use for next call *))

        val ofThread : thread_id -> transaction url (* user-specific URL to a thread on Gmail *)
    end

    structure Calendar : sig
        structure Calendars : sig
            val list : transaction (list calendar)
        end

        structure Events : sig
            val list : calendar_id -> {Min : option time, Max : option time} -> transaction (list event)
            val insert : calendar_id -> {SendUpdates : option updatesMode} -> newEvent -> transaction event
            val update : calendar_id -> event -> transaction event
            val delete : calendar_id -> event_id -> transaction unit
        end
    end
end
