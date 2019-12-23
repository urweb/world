open Json
open Urls

functor TwoLegged(M : sig
                        val api_key : string
                    end) = struct
    open M

    val token = return (Some api_key)
end

type name = {
     GivenName : option string,
     FamilyName : option string,
     FullName : option string
}
val _ : json name = json_record {GivenName = "givenName",
                                 FamilyName = "familyName",
                                 FullName = "fullName"}

type geo = {
     City : option string,
     State : option string,
     Country : option string,
     Lat : option float,
     Lng : option float
}
val _ : json geo = json_record {City = "city",
                                State = "state",
                                Country = "country",
                                Lat = "lat",
                                Lng = "lng"}

type employment = {
     Nam : option string,
     Title : option string,
     Role : option string,
     SubRole : option string,
     Seniority : option string,
     Domain : option string
}
val _ : json employment = json_record {Nam = "name",
                                       Title = "title",
                                       Role = "role",
                                       SubRole = "subRole",
                                       Seniority = "seniority",
                                       Domain = "domain"}

type facebook = {
     Handle : option string
}
val _ : json facebook = json_record {Handle = "handle"}

type github = {
     Handle : option string,
     Id : option int,
     Avatar : option string,
     Company : option string,
     Blog : option string,
     Followers : option int,
     Following : option int,
}
val _ : json github = json_record {Handle = "handle",
                                   Id = "id",
                                   Avatar = "avatar",
                                   Company = "company",
                                   Blog = "blog",
                                   Followers = "followers",
                                   Following = "following"}

type twitter = {
     Handle : option string,
     Id : option int,
     Followers : option int,
     Following : option int,
     Location : option string,
     Site : option string,
     Statuses : option int,
     Favorites : option int,
     Avatar : option string
}
val _ : json twitter = json_record {Handle = "handle",
                                    Id = "id",
                                    Followers = "followers",
                                    Following = "following",
                                    Location = "location",
                                    Site = "site",
                                    Statuses = "statuses",
                                    Favorites = "favorites",
                                    Avatar = "avatar"}

type linkedin = {
     Handle : option string
}
val _ : json linkedin = json_record {Handle = "handle"}

type googleplus = {
     Handle : option string
}
val _ : json googleplus = json_record {Handle = "handle"}

type gravatar_url = {
     Value : option string,
     Title : option string
}
val _ : json gravatar_url = json_record {Value = "value",
                                         Title = "title"}

type gravatar = {
     Handle : option string,
     Urls : list gravatar_url,
     Avatar : option string,
     Avatars : list gravatar_url
}
val _ : json gravatar = json_record {Handle = "handle",
                                     Urls = "urls",
                                     Avatar = "avatar",
                                     Avatars = "avatars"}

type person = {
     Id : option string,
     Nam : name,
     Location : option string,
     TimeZone : option string,
     UtcOffset : option int,
     Geo : geo,
     Bio : option string,
     Site : option string,
     Avatar : option string,
     Employment : employment,
     Facebook : facebook,
     Github : github,
     Twitter : twitter,
     Linkedin : linkedin,
     Googleplus : googleplus,
     Gravatar : gravatar,
     Fuzzy : option bool,
     EmailProvider : option bool,
     IndexedAt : option time
}
val _ : json person = json_record {Id = "id",
                                   Nam = "name",
                                   Location = "location",
                                   TimeZone = "timeZone",
                                   UtcOffset = "utcOffset",
                                   Geo = "geo",
                                   Bio = "bio",
                                   Site = "site",
                                   Avatar = "avatar",
                                   Employment = "employment",
                                   Facebook = "facebook",
                                   Github = "github",
                                   Twitter = "twitter",
                                   Linkedin = "linkedin",
                                   Googleplus = "googleplus",
                                   Gravatar = "gravatar",
                                   Fuzzy = "fuzzy",
                                   EmailProvider = "emailProvider",
                                   IndexedAt = "indexedAt"}

datatype response a =
         Answer of a
       | NotFound
       | LookingUpAsynchronously

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    val token =
        tok <- M.token;
        case tok of
            Some tok => return tok
          | None => error <xml>How odd: no Clearbit token!</xml>
         
    fun api url =
        tok <- token;
        WorldFfi.get url (Some ("Bearer " ^ tok)) True
         
    structure Person = struct
        fun lookup {Email = email} =
            s <- api (bless ("https://person.clearbit.com/v2/people/find?email=" ^ urlencode email));
            code <- WorldFfi.lastErrorCode;
            case code of
                200 => return (Answer (Json.fromJson s))
              | 202 => return LookingUpAsynchronously
              | 404 => return NotFound
              | _ => error <xml>Error response from Clearbit: {[s]}</xml>
    end
end
