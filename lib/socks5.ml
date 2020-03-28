open Unix
open Sys

module Misc = struct
  let retransmit fdin fdout =
    let buffer_size = 4096 in
    let buffer = Bytes.create buffer_size in
    let rec copy () = match read fdin buffer 0 buffer_size with
      | 0 -> ()
      | n -> ignore (write fdout buffer 0 n); copy ()
    in
    copy ()

  let rec restart_on_EINTR f x =
    try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x

  let try_finalize f x finally y =
    let res = try f x with exn -> finally y; raise exn in
    finally y;
    res

  let double_fork_treatment server service (client_descr, _ as client) =
    let treat () = match fork () with
      | 0 ->
        if fork () <> 0 then exit 0;
        close server; service client; exit 0
      | k ->
        ignore (restart_on_EINTR (waitpid []) k)
    in
    try_finalize treat () close client_descr

  let install_tcp_server_socket addr =
    let s = socket PF_INET SOCK_STREAM 0 in
    try
      bind s addr;
      listen s 10;
      s
    with z -> close s; raise z

  let tcp_server treat_connection addr =
    ignore (signal sigpipe Signal_ignore);
    let server_sock = install_tcp_server_socket addr in
    while true do
      print_endline "[tcp_server] will accept client...";
      let client = restart_on_EINTR accept server_sock in
      print_endline "[tcp_server] accepted client...";
      treat_connection server_sock client;
      print_endline "[tcp_server] after treating client...";
    done
end

let client () =
  print_endline "socks5 client starts...";
  let server_name = gethostname ()
  and port_number = 5678 in
  let server_addr =
    try (gethostbyname server_name).h_addr_list.(0)
    with Not_found ->
      prerr_endline (server_name ^ ": Host not found");
      exit 2 in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET(server_addr, port_number));
  match fork () with
  | 0 ->
    Misc.retransmit stdin sock;
    shutdown sock SHUTDOWN_SEND;
    exit 0
  | _ ->
    Misc.retransmit sock stdout;
    close stdout;
    wait ()

let server () =
  print_endline "socks5 server starts...";
  if Array.length Sys.argv < 2 then begin
    prerr_endline "Usage: server <port>";
    exit 2;
  end;
  let port = int_of_string Sys.argv.(1) in
  let host = (gethostbyname(gethostname ())).h_addr_list.(0) in 
  let addr = ADDR_INET (host, port) in
  let treat sock (_, client_addr as client) =
    (* log information *)
    begin match client_addr with
      | ADDR_INET(caller, _) ->
        prerr_endline ("Connection from " ^ string_of_inet_addr caller);
      | ADDR_UNIX _ ->
        prerr_endline "Connection from the Unix domain (???)";
    end;
    (* connection treatment *)
    let service (s, _) =
      dup2 s stdin; dup2 s stdout; dup2 s stderr; close s;
      execvp "pwd" [|"pwd"|]
    in
    Misc.double_fork_treatment sock service client in
  Misc.tcp_server treat addr
