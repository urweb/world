(* For this demo, it's necessary to create influxdbSecrets.ur. *)
structure I = Influxdb.Make(Influxdb.TwoLegged(InfluxdbSecrets))

fun addRows () =
    WorldFfi.allowHttp;
    I.write {Precision = None}
    ({Measurement = "test1",
      Tags = ("tag1", "t1") :: ("tag2", "t2") :: [],
      Fields = ("field1", "value1") :: ("field2", "uh\\oh\"!") :: [],
      Timestamp = None}
  :: {Measurement = "test1",
      Tags = [],
      Fields = ("field3", "value3") :: [],
      Timestamp = Some 12345678}
  :: {Measurement = "test1",
      Tags = ("tag4", "t4") :: [],
      Fields = ("field4", "yikes\nyowza") :: [],
      Timestamp = None}
  :: []);
    return <xml><body>Done.</body></xml>

fun main () = return <xml><body>
  <form>
    <submit action={addRows}/>
  </form>
</body></xml>
