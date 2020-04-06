open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let _meth = req |> Request.meth |> Code.string_of_method in
    let _headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun _body -> "Hello, you are requesting " ^ uri ^ "\n")
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 80)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
