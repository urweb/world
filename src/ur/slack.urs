structure Scope : sig
    type t
    val empty : t
    val union : t -> t -> t
    val readonly : t -> bool

    val channelsHistory : t
    val channelsManage : t
    val channelsRead : t
end

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) : sig
    val token : transaction (option string)
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
     PendingShared : list string,
     IsPendingExtShared : bool,
     IsMember : bool,
     IsPrivate : bool,
     IsMpim : bool,
     LastRead : option time,
     Topic : topic_or_purpose,
     Purpose : topic_or_purpose,
     PreviousNames : list string,
     NumMembers : int,
     Locale : option string
}

functor Make(M : AUTH) : sig
    structure Conversations : sig
        val list : transaction (list conversation)
    end
end
