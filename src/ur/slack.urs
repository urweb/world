structure Scope : sig
    type t
    val empty : t
    val union : t -> t -> t
    val readonly : t -> bool

    val channelsHistory : t
    val channelsManage : t
    val channelsRead : t
    val channelsWrite : t
    val identityBasic : t
    val identityEmail : t
end

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) : sig
    val token : transaction (option string)
end
functor ThreeLegged(M : sig
                        val client_id : string
                        val client_secret : string
                        val https : bool

                        val scopes : Scope.t
                        val onCompletion : transaction page
                    end) : sig
    val token : transaction (option string)
    val authorize : transaction page
    val status : transaction xbody
    val logout : transaction unit
end

type topic_or_purpose = {
     Value : string,
     Creator : string,
     LastSet : time
}

type conversation = {
     Id : string,
     Nam : string,
     IsChannel : bool,
     IsGroup : bool,
     IsIm : bool,
     Created : time,
     Creator : string,
     IsArchived : bool,
     IsGeneral : bool,
     Unlinked : int,
     NameNormalized : string,
     IsReadOnly : option bool,
     IsShared : bool,
     IsExtShared : bool,
     IsOrgShared : bool,
     SharedTeamIds : option (list string),
     PendingShared : list string,
     IsPendingExtShared : bool,
     IsMember : bool,
     IsPrivate : bool,
     IsMpim : bool,
     LastRead : option string,
     Topic : topic_or_purpose,
     Purpose : topic_or_purpose,
     PreviousNames : list string,
     NumMembers : option int,
     Locale : option string
}

type edited = {
     User : string,
     Ts : string
}

type reaction = {
     Nam : string,
     Count : int,
     Users : list string
}

type message = {
     Typ : string,
     Subtype : option string,
     Channel : option string,
     User : string,
     Text : string,
     Ts : string,
     Edited : option edited,
     Hidden : option bool,
     IsStarred : option bool,
     PinnedTo : option (list string),
     Reactions : option (list reaction)
}

type profile = {
     Title : string,
     Phone : string,
     Skype : string,
     RealName : string,
     RealNameNormalized : string,
     DisplayName : string,
     DisplayNameNormalized : string,
     StatusText : string,
     StatusEmoji : string,
     StatusExpiration : time,
     AvatarHash : string,
     FirstName : option string,
     LastName : option string,
     Email : option string,
     ImageOriginal : option string,
     Image24 : option string,
     Image32 : option string,
     Image48 : option string,
     Image72 : option string,
     Image192 : option string,
     Image512 : option string,
     Team : string
}

type user = {
     Id : string,
     TeamId : string,
     Nam : string,
     Deleted : bool,
     Color : string,
     RealName : string,
     Tz : string,
     TzLabel : string,
     TzOffset : int,
     Profile : profile,
     IsAdmin : bool,
     IsOwner : bool,
     IsPrimaryOwner : bool,
     IsRestricted : bool,
     IsUltraRestricted : bool,
     IsBot : bool,
     IsStranger : option bool,
     Updated : time,
     IsAppUser : bool,
     IsInvitedUser : option bool,
     Has2fa : option bool,
     Locale : option string
}

type identity = {
     Nam : string,
     Email : option string
}

functor Make(M : AUTH) : sig
    structure Conversations : sig
        val list : transaction (list conversation)
        val history : string (* conversation ID *) -> transaction (list message)
        val create : string (* channel name *) -> transaction conversation
    end

    structure Chat : sig
        val postMessage : {Channel : string (* ID *),
                           Text : string} -> transaction message
    end

    structure Users : sig
        val info : string (* user ID *) -> transaction user
        val identity : transaction (option identity)
    end
end

val suggestChannelName : string -> string
(* Create a version of an arbitrary string that works as a Slack channel name,
 * doing our best to preserve readability. *)

val channelUrl : {Channel : string (* conversation ID *), Team : option string (* workspace ID *)} -> url
