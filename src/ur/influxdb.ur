open Json

type params = {InfluxdbUrl : string,
               Org : string,
               Bucket : string,
               ApiToken : string}
val _ : json params = json_record {InfluxdbUrl = "InfluxdbUrl",
                                   Org = "Org",
                                   Bucket = "Bucket",
                                   ApiToken = "ApiToken"}

functor TwoLegged(M : sig
                      val influxdb_url : string
                      val org : string
                      val bucket : string
                      val api_token : string
                    end) = struct
    val token = return (Some (toJson {InfluxdbUrl = M.influxdb_url,
                                      Org = M.org,
                                      Bucket = M.bucket,
                                      ApiToken = M.api_token}))
end

type line = {Measurement : string,
             Tags : list (string * string),
             Fields : list (string * string),
             Timestamp : option int}
type lines = list line

datatype precision = S | Ms | Us | Ns
val _ : show precision = mkShow (fn v =>
                                    case v of
                                        S => "s"
                                      | Ms => "ms"
                                      | Us => "us"
                                      | Ns => "ns")

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    fun showId id =
        if String.all (fn ch => Char.isAlnum ch || ch = #"_" || ch = #"-") id then
            id
        else
            error <xml>Invalid InfluxDB identifier "{[id]}"</xml>

    fun showLine line =
        let
            val s = showId line.Measurement
            val s = List.foldl (fn (k, v) s =>
                                   s ^ "," ^ showId k ^ "=" ^ toJson v)
                               s line.Tags
            val s = case line.Fields of
                        [] => error <xml>InfluxDB line contains no fields.</xml>
                      | (k, v) :: fs => List.foldl (fn (k, v) s =>
                                                       s ^ "," ^ showId k ^ "=" ^ toJson v)
                                                   (s ^ " " ^ showId k ^ "=" ^ toJson v) fs
        in
            case line.Timestamp of
                None => s
              | Some ts => s ^ " " ^ show ts
        end

    val showLines =
        List.foldl (fn line s => s ^ showLine line ^ "\n") ""

    fun url make =
        params <- M.token;
        case params of
            None => error <xml>No InfluxDB token</xml>
          | Some params =>
            params <- return (fromJson params : params);
            prefix <- return (if params.InfluxdbUrl <> ""
                                 && String.sub params.InfluxdbUrl (String.length params.InfluxdbUrl - 1) = #"/" then
                                  params.InfluxdbUrl ^ "api/v2/"
                              else
                                  params.InfluxdbUrl ^ "/api/v2/");
            s <- return (make params);
            return (params, bless (prefix ^ s))

    fun post make body =
        (params, url) <- url make;
        debug ("  InfluxDB request to: " ^ show url);
        debug ("InfluxDB request body: " ^ body);
        Monad.ignore (WorldFfi.post url
                                    (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Token " ^ params.ApiToken))
                                    (Some "text/plain; charset=utf-8")
                                    body)

    fun write settings lines =
        post (fn params => "write?org=" ^ Urls.urlencode params.Org
                           ^ "&bucket=" ^ Urls.urlencode params.Bucket
                           ^ (case settings.Precision of
                                  None => ""
                                | Some prec => "&precision=" ^ show prec))
             (showLines lines)
end
