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

type person_geo = {
     City : option string,
     State : option string,
     Country : option string,
     Lat : option float,
     Lng : option float
}
val _ : json person_geo = json_record {City = "city",
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

type person_facebook = {
     Handle : option string
}
val _ : json person_facebook = json_record {Handle = "handle"}

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

type person_twitter = {
     Handle : option string,
     Id : option string,
     Followers : option int,
     Following : option int,
     Location : option string,
     Site : option string,
     Statuses : option int,
     Favorites : option int,
     Avatar : option string
}
val _ : json person_twitter = json_record {Handle = "handle",
                                           Id = "id",
                                           Followers = "followers",
                                           Following = "following",
                                           Location = "location",
                                           Site = "site",
                                           Statuses = "statuses",
                                           Favorites = "favorites",
                                           Avatar = "avatar"}

type person_linkedin = {
     Handle : option string
}
val _ : json person_linkedin = json_record {Handle = "handle"}

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

type site = {
     PhoneNumbers : list string,
     EmailAddresses : list string
}
val _ : json site = json_record {PhoneNumbers = "phoneNumbers",
                                 EmailAddresses = "emailAddresses"}

type category = {
     Sector : option string,
     IndustryGroup : option string,
     Industry : option string,
     SubIndustry : option string,
     SicCode : option string,
     NaicsCode : option string
}
val _ : json category = json_record {Sector = "sector",
                                     IndustryGroup = "industryGroup",
                                     Industry = "industry",
                                     SubIndustry = "subIndustry",
                                     SicCode = "sicCode",
                                     NaicsCode = "naicsCode"}

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
val _ : json company_geo = json_record {StreetNumber = "streetNumber",
                                        StreetName = "streetName",
                                        SubPremise = "subPremise",
                                        City = "city",
                                        State = "state",
                                        StateCode = "stateCode",
                                        PostalCode = "postalCode",
                                        Country = "country",
                                        CountryCode = "countryCode",
                                        Lat = "lat",
                                        Lng = "lng"}

type identifiers = {
     UsEIN : option string
}
val _ : json identifiers = json_record {UsEIN = "usEIN"}

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
val _ : json metrics = json_record {Raised = "raised",
                                    AlexaUsRank = "alexaUsRank",
                                    AlexaGlobalRank = "alexaGlobalRank",
                                    Employees = "employees",
                                    EmployeesRange = "employeesRange",
                                    MarketCap = "marketCap",
                                    AnnualRevenue = "annualRevenue",
                                    EstimatedAnnualRevenue = "estimatedAnnualRevenue",
                                    FiscalYearEnd = "fiscalYearEnd"}

type company_facebook = {
     Handle : option string
}
val _ : json company_facebook = json_record {Handle = "handle"}

type company_linkedin = {
     Handle : option string
}
val _ : json company_linkedin = json_record {Handle = "handle"}

type company_twitter = {
     Handle : option string,
     Id : option string,
     Bio : option string,
     Followers : option int,
     Following : option int,
     Location : option string,
     Site : option string,
     Avatar : option string
}
val _ : json company_twitter = json_record {Handle = "handle",
                                            Id = "id",
                                            Bio = "bio",
                                            Followers = "followers",
                                            Following = "following",
                                            Location = "location",
                                            Site = "site",
                                            Avatar = "avatar"}

type crunchbase = {
     Handle : option string
}
val _ : json crunchbase = json_record {Handle = "handle"}

type parent = {
     Domain : option string
}
val _ : json parent = json_record {Domain = "domain"}

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
     IndexedAt : option time
}
val _ : json company = json_record {Id = "id",
                                    Nam = "name",
                                    LegalName = "legalName",
                                    Domain = "domain",
                                    DomainAliases = "domainAliases",
                                    Site = "site",
                                    Tags = "tags",
                                    Category = "category",
                                    Description = "description",
                                    FoundedYear = "foundedYear",
                                    Location = "location",
                                    TimeZone = "timeZone",
                                    UtcOffset = "utcOffset",
                                    Geo = "geo",
                                    Identifiers = "identifiers",
                                    Metrics = "metrics",
                                    Facebook = "facebook",
                                    LinkedIn = "linkedin",
                                    Twitter = "twitter",
                                    Crunchbase = "crunchbase",
                                    Logo = "logo",
                                    EmailProvider = "emailProvider",
                                    Typ = "type",
                                    Phone = "phone",
                                    Tech = "tech",
                                    TechCategories = "techCategories",
                                    Parent = "parent",
                                    IndexedAt = "indexedAt"}

datatype response a =
         Answer of a
       | MalformedName
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
              | 422 => return MalformedName
              | _ => error <xml>Error code #{[code]} from Clearbit for person: {[s]}</xml>
    end

    structure Company = struct
        fun lookup {Domain = domain} =
            s <- api (bless ("https://company.clearbit.com/v2/companies/find?domain=" ^ urlencode domain));
            code <- WorldFfi.lastErrorCode;
            case code of
                200 => return (Answer (Json.fromJson s))
              | 202 => return LookingUpAsynchronously
              | 404 => return NotFound
              | 422 => return MalformedName
              | _ => error <xml>Error code #{[code]} from Clearbit for company: {[s]}</xml>
    end
end
