open Json

table users : { Login : string,
                AvatarUrl : string,
                Nam : option string,
                Company : option string,
                Blog : option string,
                Location : option string,
                Email : option string,
                Hireable : option bool,
                Bio : option string }
  PRIMARY KEY Login

con users_hidden_constraints = _
constraint [Pkey = [Login]] ~ users_hidden_constraints

table secrets : { Login : string,
                  Secret : int }
  PRIMARY KEY Login,
  CONSTRAINT Login FOREIGN KEY Login REFERENCES users(Login) ON DELETE CASCADE

cookie user : { Login : string, Secret : int }

signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

type profile = { Login : string,
                 AvatarUrl : string,
                 Nam : option string,
                 Company : option string,
                 Blog : option string,
                 Location : option string,
                 Email : option string,
                 Hireable : option bool,
                 Bio : option string }

val json_profile : json profile =
    json_record {Login = "login",
                 AvatarUrl = "avatar_url",
                 Nam = "name",
                 Company = "company",
                 Blog = "blog",
                 Location = "location",
                 Email = "email",
                 Hireable = "hireable",
                 Bio = "bio"}

functor Make(M : S) = struct
    open M

    fun withToken tok =
        profile <- WorldFfi.get (bless "https://api.github.com/user") (Some tok);
        debug profile;
        (profile : profile) <- return (Json.fromJson profile);
        exists <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM users
                            WHERE users.Login = {[profile.Login]});
        secret <-
        (if exists then
             dml (UPDATE users
                  SET AvatarUrl = {[profile.AvatarUrl]},
                    Nam = {[profile.Nam]},
                    Company = {[profile.Company]},
                    Blog = {[profile.Blog]},
                    Location = {[profile.Location]},
                    Email = {[profile.Email]},
                    Hireable = {[profile.Hireable]},
                    Bio = {[profile.Bio]}
                  WHERE Login = {[profile.Login]});
             oneRowE1 (SELECT (secrets.Secret)
                       FROM secrets
                       WHERE secrets.Login = {[profile.Login]})
         else
             dml (INSERT INTO users(Login, AvatarUrl, Nam, Company, Blog, Location,
                      Email, Hireable, Bio)
                  VALUES ({[profile.Login]}, {[profile.AvatarUrl]}, {[profile.Nam]},
                      {[profile.Company]}, {[profile.Blog]}, {[profile.Location]},
                      {[profile.Email]}, {[profile.Hireable]}, {[profile.Bio]}));
             secret <- rand;
             dml (INSERT INTO secrets(Login, Secret)
                  VALUES ({[profile.Login]}, {[secret]}));
             return secret);

        setCookie user {Value = {Login = profile.Login, Secret = secret},
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M

                        val authorize_url = bless "https://github.com/login/oauth/authorize"
                        val access_token_url = bless "https://github.com/login/oauth/access_token"
                        val withToken = withToken
                    end)

    val whoami =
        c <- getCookie user;
        case c of
            None => return None
          | Some r =>
            ok <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM secrets
                            WHERE secrets.Login = {[r.Login]}
                              AND secrets.Secret = {[r.Secret]});
            if ok then
                return (Some r.Login)
            else
                error <xml>Invalid login information</xml>
end
