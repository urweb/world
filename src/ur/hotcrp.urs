(* A system to manage conference reviewing; see <https://www.hotcrp.com/>. *)

datatype status =
         Inprogress
       | Withdrawn
       | Submitted
       | Accepted
       | Rejected

type author = {
     Email : string,
     First : option string,
     Last : option string,
     Affiliation : option string,
     Contact : option bool
}
val json_author : Json.json author

type submission = {
     Mimetype : string,
     Hash : string,
     Timestamp : time,
     Size : int
}
val json_submission : Json.json submission

type paper = {
     Pid : int,
     Title : string,
     Status : status,
     Submitted : option bool,
     SubmittedAt : option time,
     Withdrawn : option bool,
     WithdrawnAt : option time,
     Authors : option (list author),
     Abstract : string,
     Submission : option submission,
     Collaborators : option string
}
val json_paper : Json.json paper
