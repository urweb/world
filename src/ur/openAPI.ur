open Json

(** * OpenAPI format (read as YAML or JSON) *)

type server = {
     Url : string
}
val _ : json server = json_record {Url = "url"}

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
structure Schema = Json.Recursive(struct
                                      con t = schema
                                      fun json_t [a] (_ : json a) : json (t a) =
                                          json_record_withOptional {}
                                          {Ref = "$ref",
                                           Typ = "type",
                                           Format = "format",
                                           Items = "items",
                                           Required = "required",
                                           Properties = "properties",
                                           AllOf = "allOf",
                                           AnyOf = "anyOf",
                                           OneOf = "oneOf"}
                                  end)

type parameter = {
     Nam : option string,
     In : option string,
     Description : option string,
     Schema : option Schema.r,
     Required : option bool,
     Ref : option string
}
val _ : json parameter = json_record_withOptional {}
                         {Nam = "name",
                          In = "in",
                          Schema = "schema",
                          Description = "description",
                          Required = "required",
                          Ref = "$ref"}

type content = {
     Schema : option Schema.r
}
val _ : json content = json_record_withOptional {}
                                                {Schema = "schema"}

type response = {
     Description : option string,
     Content : option (list (string * content))
}
val _ : json response = json_record_withOptional {}
                        {Description = "description",
                         Content = "content"}

type request_body = {
     Required : option bool,
     Content : option (list (string * content))
}
val _ : json request_body = json_record_withOptional {}
                                                     {Content = "content",
                                                      Required = "required"}

type security = list (list (string * list string))

type method = {
     Description : option string,
     Parameters : option (list parameter),
     RequestBody : option request_body,
     Responses : option (list (string * response)),
     Security : option security
}
val _ : json method = json_record_withOptional {}
                                               {Description = "description",
                                                Parameters = "parameters",
                                                RequestBody = "requestBody",
                                                Responses = "responses",
                                                Security = "security"}

type path = {
     Parameters : option (list parameter),
     Get : option method,
     Post : option method,
     Put : option method,
     Delete : option method,
     Patch : option method,
}
val _ : json path = json_record_withOptional {} {Parameters = "parameters",
                                                 Get = "get",
                                                 Post = "post",
                                                 Put = "put",
                                                 Delete = "delete",
                                                 Patch = "patch"}

type info = {
     Title : string,
     Description : option string
}
val _ : json info = json_record_withOptional {Title = "title"}
                                             {Description = "description"}

type flow = {
     AuthorizationUrl : string,
     TokenUrl : string,
     Scopes : list (string * string)
}
val _ : json flow = json_record {AuthorizationUrl = "authorizationUrl",
                                 TokenUrl = "tokenUrl",
                                 Scopes = "scopes"}

type flows = {
     AuthorizationCode : option flow
}
val _ : json flows = json_record_withOptional {}
                     {AuthorizationCode = "authorizationCode"}

type security_scheme = {
     Typ : string,
     Nam : option string,
     In : option string,
     Flows : option flows
}
val _ : json security_scheme = json_record_withOptional {Typ = "type"}
                                                        {Nam = "name",
                                                         In = "in",
                                                         Flows = "flows"}

type components = {
     Schemas : option (list (string * Schema.r)),
     SecuritySchemes : option (list (string * security_scheme)),
     Parameters : option (list (string * parameter))
}
val _ : json components = json_record_withOptional {}
                                                   {Schemas = "schemas",
                                                    SecuritySchemes = "securitySchemes",
                                                    Parameters = "parameters"}

type openapi_spec = {
     Version : string,
     Info : info,
     Components : option components,
     Servers : option (list server),
     Paths : list (string * path),
     Security : option security
}
val json_spec : json openapi_spec = json_record_withOptional {Version = "openapi",
                                                              Info = "info",
                                                              Paths = "paths"}
                                                             {Servers = "servers",
                                                              Components = "components",
                                                              Security = "security"}
