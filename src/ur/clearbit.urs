functor TwoLegged(M : sig
                        val api_key : string
                    end) : sig
    val token : transaction (option string)
end

type name = {
     GivenName : option string,
     FamilyName : option string,
     FullName : option string
}

type geo = {
     City : option string,
     State : option string,
     Country : option string,
     Lat : option float,
     Lng : option float
}
     
type employment = {
     Nam : option string,
     Title : option string,
     Role : option string,
     SubRole : option string,
     Seniority : option string,
     Domain : option string
}

type facebook = {
     Handle : option string
}

type github = {
     Handle : option string,
     Id : option int,
     Avatar : option string,
     Company : option string,
     Blog : option string,
     Followers : option int,
     Following : option int,
}

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

type linkedin = {
     Handle : option string
}

type googleplus = {
     Handle : option string
}

type gravatar_url = {
     Value : option string,
     Title : option string
}

type gravatar = {
     Handle : option string,
     Urls : list gravatar_url,
     Avatar : option string,
     Avatars : list gravatar_url
}

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
                           
datatype response a =
         Answer of a
       | NotFound
       | LookingUpAsynchronously

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure Person : sig
        val lookup : {Email : string} -> transaction (response person)
    end
end
