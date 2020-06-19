structure O = OpenIdConnect.Make(struct
                                     open OidcSecrets

                                     val authorize_url = bless "https://oidc.csail.mit.edu/authorize"
                                     val access_token_url = bless "https://oidc.csail.mit.edu/token"
                                     val userinfo_url = bless "https://oidc.csail.mit.edu/userinfo"
                                     val https = False
                                     val onCompletion = return <xml>It worked.</xml>
                                 end)

val main =
    email <- O.whoami;
    name <- O.name;
    return <xml><body>
      <a link={O.authorize}>Authorize</a>
      <hr/>
      {case email of
           None => <xml></xml>
         | Some email => <xml><p><b>E-mail:</b> {[email]}</p></xml>}
      {case name of
           None => <xml></xml>
         | Some name => <xml><p><b>Name:</b> {[name]}</p></xml>}
    </body></xml>
