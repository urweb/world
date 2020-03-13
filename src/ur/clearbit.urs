functor TwoLegged(M : sig
                        val api_key : string
                    end) : sig
    val token : transaction (option string)
end

(** * Person API records *)

type name = {
     GivenName : option string,
     FamilyName : option string,
     FullName : option string
}

type person_geo = {
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

type person_facebook = {
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

type person_twitter = {
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

type person_linkedin = {
     Handle : option string
}

type googleplus = {
     Handle : option string
}

type gravatar_url = {
     Value : option string,
     Title : option string
}

type gravatar_avatar = {
     Url : option string,
     Typ : option string
}

type gravatar = {
     Handle : option string,
     Urls : list gravatar_url,
     Avatar : option string,
     Avatars : list gravatar_avatar
}

type person = {
     Id : option string,
     Nam : name,
     Location : option string,
     TimeZone : option string,
     UtcOffset : option int,
     Geo : person_geo,
     Bio : option string,
     Site : option string,
     Avatar : option string,
     Employment : employment,
     Facebook : person_facebook,
     Github : github,
     Twitter : person_twitter,
     Linkedin : person_linkedin,
     Googleplus : googleplus,
     Gravatar : gravatar,
     Fuzzy : option bool,
     EmailProvider : option bool,
     IndexedAt : option time
}

(** * Company API records *)

type site = {
     PhoneNumbers : list string,
     EmailAddresses : list string
}

type category = {
     Sector : option string,
     IndustryGroup : option string,
     Industry : option string,
     SubIndustry : option string,
     SicCode : option string,
     NaicsCode : option string
}

type company_geo = {
     StreetNumber : option string,
     StreetName : option string,
     SubPremise : option string,
     City : option string,
     State : option string,
     StateCode : option string,
     PostalCode : option string,
     Country : option string,
     CountryCode : option string,
     Lat : option float,
     Lng : option float
}

type identifiers = {
     UsEIN : option string
}

type metrics = {
     Raised : option int,
     AlexaUsRank : option int,
     AlexaGlobalRank : option int,
     Employees : option int,
     EmployeesRange : option string,
     MarketCap : option int,
     AnnualRevenue : option int,
     EstimatedAnnualRevenue : option string,
     FiscalYearEnd : option int
}

type company_facebook = {
     Handle : option string
}

type company_linkedin = {
     Handle : option string
}

type company_twitter = {
     Handle : option string,
     Id : option int,
     Bio : option string,
     Followers : option int,
     Following : option int,
     Location : option string,
     Site : option string,
     Avatar : option string
}

type crunchbase = {
     Handle : option string
}

type parent = {
     Domain : option string
}

type ultimate_parent = {
     Domain : option string
}

type company = {
     Id : option string,
     Nam : option string,
     LegalName : option string,
     Domain : option string,
     DomainAliases : list string,
     Site : site,
     Tags : list string,
     Category : category,
     Description : option string,
     FoundedYear : option int,
     Location : option string,
     TimeZone : option string,
     UtcOffset : option int,
     Geo : company_geo,
     Identifiers : identifiers,
     Metrics : metrics,
     Facebook : company_facebook,
     LinkedIn : company_linkedin,
     Twitter : company_twitter,
     Crunchbase : crunchbase,
     Logo : option string,
     EmailProvider : option bool,
     Typ : option string,
     Phone : option string,
     Tech : list string,
     TechCategories : list string,
     Parent : parent,
     UltimateParent : ultimate_parent,
     IndexedAt : option time
}

(** * Now for the actual methods.... *)

datatype response a =
         Answer of a
       | MalformedName
       | NotFound
       | LookingUpAsynchronously

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure Person : sig
        val lookup : {Email : string} -> transaction (response person)
    end

    structure Company : sig
        val lookup : {Domain : string} -> transaction (response company)
    end
end
