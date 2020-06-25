open Json

structure Scope = struct
    type t = Scopes.t [ChannelsHistory, ChannelsManage, ChannelsWrite, ChannelsRead]
    val empty = Scopes.empty
    val union = Scopes.union
    val toString = Scopes.toString {ChannelsHistory = "channels:history",
                                    ChannelsManage = "channels:manage",
                                    ChannelsWrite = "channels:write",
                                    ChannelsRead = "channels:read"}

    val channelsHistory = Scopes.one [#ChannelsHistory]
    val channelsManage = Scopes.one [#ChannelsManage]
    val channelsRead = Scopes.one [#ChannelsRead]
    val channelsWrite = Scopes.one [#ChannelsWrite]

    val readonly = Scopes.disjoint (union channelsWrite channelsManage)
end

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) = struct
    open M

    val token = return (Some token)
end

val _ : json time = json_derived (addSeconds minTime) toSeconds

type topic_or_purpose = {
     Value : string,
     Creator : string,
     LastSet : time
}
val _ : json topic_or_purpose = json_record {Value = "value",
                                             Creator = "creator",
                                             LastSet = "last_set"}

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
val _ : json conversation = json_record_withOptional
                                {Id = "id",
                                 Nam = "name",
                                 IsChannel = "is_channel",
                                 IsGroup = "is_group",
                                 IsIm = "is_im",
                                 Created = "created",
                                 Creator = "creator",
                                 IsArchived = "is_archived",
                                 IsGeneral = "is_general",
                                 Unlinked = "unlinked",
                                 NameNormalized = "name_normalized",
                                 IsShared = "is_shared",
                                 IsExtShared = "is_ext_shared",
                                 IsOrgShared = "is_org_shared",
                                 PendingShared = "pending_shared",
                                 IsPendingExtShared = "is_pending_ext_shared",
                                 IsMember = "is_member",
                                 IsPrivate = "is_private",
                                 IsMpim = "is_mpim",
                                 Topic = "topic",
                                 Purpose = "purpose",
                                 PreviousNames = "previous_names"}
                                {IsReadOnly = "is_read_only",
                                 LastRead = "last_read",
                                 Locale = "locale",
                                 SharedTeamIds = "shared_team_ids",
                                 NumMembers = "num_members"}

type edited = {
     User : string,
     Ts : string
}
val _ : json edited = json_record {User = "user",
                                   Ts = "ts"}

type reaction = {
     Nam : string,
     Count : int,
     Users : list string
}
val _ : json reaction = json_record {Nam = "name",
                                     Count = "count",
                                     Users = "users"}

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
val _ : json message = json_record_withOptional
                           {Typ = "type",
                            User = "user",
                            Text = "text",
                            Ts = "ts"}
                           {Subtype = "subtype",
                            Channel = "channel",
                            Edited = "edited",
                            Hidden = "hidden",
                            IsStarred = "is_starred",
                            PinnedTo = "pinned_to",
                            Reactions = "reactions"}


functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Slack to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://slack.com/api/"

    type slack_response = {
         Ok : bool,
         Error : option string,
    }
    val _ : json slack_response = json_record_withOptional {Ok = "ok"}
                                                           {Error = "error"}

    fun wrap_errcheck (t : transaction string) =
        s <- t;
        debug ("Response: " ^ s);
        sr <- return (fromJson s : slack_response);
        if sr.Ok then
            return s
        else
            error <xml>Slack API error: {[sr.Error]}</xml>

    fun api url =
        tok <- token;
        wrap_errcheck (WorldFfi.get (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False)

    fun apiOpt url =
        tok <- token;
        WorldFfi.getOpt (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) False

    fun apiPost url =
        tok <- token;
        wrap_errcheck (WorldFfi.post (bless (prefix ^ url)) (Some ("Bearer " ^ tok)) None "")

    fun oneJsonField [t ::: Type] (_ : json t) (label : string) (s : string) : t =
        let
            val j : json {Value : t} =
                json_record {Value = label}
        in
            (@fromJson j s).Value
        end

    fun apiList [t ::: Type] (_ : json t) (listLabel : string) (url : string) : transaction (list t) =
        page <- api url;
        return (oneJsonField listLabel page)

    val urlPrefix = "https://slack.com/"

    structure Conversations = struct
        val list = apiList "channels" "conversations.list"

        fun history ch = apiList "messages" ("conversations.history?channel=" ^ Urls.urlencode ch)

        fun create name =
            s <- apiPost ("conversations.create?name=" ^ Urls.urlencode name);
            return (oneJsonField "channel" s)

        fun url c = bless (urlPrefix ^ "app_redirect?channel=" ^ Urls.urlencode c.Id
                           ^ case c.SharedTeamIds of
                                 Some (tid :: _) => "&team=" ^ Urls.urlencode tid
                               | _ => "")
    end

    structure Chat = struct
        fun postMessage r =
            s <- apiPost ("chat.postMessage?channel=" ^ Urls.urlencode r.Channel
                          ^ "&text=" ^ Urls.urlencode r.Text);
            return (oneJsonField "message" s)
    end
end
