(** * OpenAPI format (read as YAML or JSON) *)

type server = {
     Url : string
}

type schema self = {
     Ref : option string,
     Typ : option string,
     Format : option string,
     Items : option self,
     Required : option (list string),
     Properties : option (list (string * self)),
     AllOf : option (list self),
     AnyOf : option (list self),
     OneOf : option (list self)
}

structure Schema : sig
    datatype r = Rec of schema r
    val json_r : Json.json r
end

type parameter = {
     Nam : option string,
     In : option string,
     Description : option string,
     Schema : option Schema.r,
     Required : option bool,
     Ref : option string
}

type content = {
     Schema : option Schema.r
}

type response = {
     Description : option string,
     Content : option (list (string * content))
}

type request_body = {
     Required : option bool,
     Content : option (list (string * content))
}

type security = list (list (string * list string))

type method = {
     Description : option string,
     Parameters : option (list parameter),
     RequestBody : option request_body,
     Responses : option (list (string * response)),
     Security : option security
}

type path = {
     Parameters : option (list parameter),
     Get : option method,
     Post : option method,
     Put : option method,
     Delete : option method,
     Patch : option method,
}

type info = {
     Title : string,
     Description : option string
}

type flow = {
     AuthorizationUrl : string,
     TokenUrl : string,
     Scopes : list (string * string)
}

type flows = {
     AuthorizationCode : option flow
}

type security_scheme = {
     Typ : string,
     Nam : option string,
     In : option string,
     Flows : option flows
}

type components = {
     Schemas : option (list (string * Schema.r)),
     SecuritySchemes : option (list (string * security_scheme)),
     Parameters : option (list (string * parameter))
}

type openapi_spec = {
     Version : string,
     Info : info,
     Components : option components,
     Servers : option (list server),
     Paths : list (string * path),
     Security : option security
}

val json_spec : Json.json openapi_spec
