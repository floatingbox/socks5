open Unix
open Sys

module Misc = struct
  let retransmit fdin fdout =
    let buffer_size = 4096 in
    let buffer = Bytes.create buffer_size in
    let rec copy () =
      match read fdin buffer 0 buffer_size with
      | 0 -> ()
      | n ->
          ignore (write fdout buffer 0 n);
          copy ()
    in
    copy ()

  let rec restart_on_EINTR f x =
    try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x

  let try_finalize f x finally y =
    let res =
      try f x
      with exn ->
        finally y;
        raise exn
    in
    finally y;
    res

  let double_fork_treatment sock service ((client_descr, _) as client) =
    let treat () =
      match fork () with
      | 0 ->
          if fork () <> 0 then exit 0;
          close sock;
          service client;
          exit 0
      | k -> ignore (restart_on_EINTR (waitpid []) k)
    in
    try_finalize treat () close client_descr

  let install_tcp_server_socket addr =
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true;
    try
      bind s addr;
      listen s 10;
      s
    with z ->
      close s;
      raise z

  let tcp_server treat_connection addr =
    ignore (signal sigpipe Signal_ignore);
    let server_sock = install_tcp_server_socket addr in
    while true do
      print_endline "[tcp_server] will accept client...";
      let client = restart_on_EINTR accept server_sock in
      print_endline "[tcp_server] accepted client...";
      treat_connection server_sock client;
      print_endline "[tcp_server] after treating client..."
    done
end

let print_bytes by =
  Bytes.iter (fun c -> Printf.printf "%02x " (int_of_char c)) by;
  print_newline ()

let ipv4_addr_to_bytes addr =
  let ss = String.split_on_char '.' addr in
  let is = List.map int_of_string ss in
  let bs = Bytes.create (List.length is) in
  let rec set i vs =
    match (i, vs) with
    | _, [] -> ()
    | i, x :: ys ->
        Bytes.set bs i (char_of_int x);
        set (i + 1) ys
  in
  set 0 is;
  bs

let ipv4_addr_of_bytes by =
  let f s i =
    let v = Bytes.get by i in
    let v = v |> int_of_char |> string_of_int in
    match s with "" -> v | _ -> s ^ "." ^ v
  in
  List.fold_left f "" [ 0; 1; 2; 3 ]

let receive sock =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let n = read sock buffer 0 buffer_size in
  Printf.printf "[recv %4d bytes] " n;
  print_bytes (Bytes.sub buffer 0 n);
  Bytes.sub buffer 0 n

let send sock buffer =
  let n = write sock buffer 0 (Bytes.length buffer) in
  Printf.printf "[sent %4d bytes] " n;
  print_bytes (Bytes.sub buffer 0 n)

module Tester = struct
  (* let request =
       let uri = Uri.of_string "localhost" in
       let%lwt (response, body) = Cohttp_lwt_unix.Client.get uri in
       assert (response.status = `OK);
       let%lwt body = Cohttp_lwt.Body.to_string body in
       print_endline body |> Lwt.return

     let do_request =
       print_endline "Tester.do_request";
       Lwt_main.run request*)

  let http_request uri socks5_addr socks5_port =
    let uri_str = uri in
    let uri = Uri.of_string uri in
    let uri =
      if Option.is_some (Uri.scheme uri) then uri
      else Uri.of_string ("http://" ^ uri_str)
    in
    print_endline ("[http_request] uri: " ^ Uri.to_string uri);
    let host = Uri.host uri |> Option.get in
    let path = Uri.path uri in
    let path = if path = "" then "/" else path in
    print_endline ("[http_request] host: " ^ host);
    let socks5_addr =
      try (gethostbyname socks5_addr).h_addr_list.(0)
      with Not_found ->
        prerr_endline (socks5_addr ^ ": Host not found");
        exit 2
    in
    let sock = socket PF_INET SOCK_STREAM 0 in
    connect sock (ADDR_INET (socks5_addr, socks5_port));
    let packet = "GET " ^ path ^ " HTTP/1.1\r\n" in
    let packet = packet ^ "Host: " ^ host ^ "\r\n" in
    let packet = packet ^ "Accept: */*\r\n" in
    let packet = packet ^ "\r\n" in
    let packet_len = String.length packet in

    let dst_addr = (gethostbyname host).h_addr_list.(0) in
    let dst_addr_len = 4 in
    let dst_addr = Core.Unix.Inet_addr.to_string dst_addr in
    print_endline ("dst_addr: " ^ dst_addr);
    let dst_addr_buf = ipv4_addr_to_bytes dst_addr in
    let dst_port =
      let scheme = Uri.scheme uri |> Option.get in
      if scheme = "http" then 80 else 443
    in
    let packet_offset = 1 + dst_addr_len + 2 in

    let buffer_len = packet_offset + packet_len in
    let buffer = Bytes.create buffer_len in
    Bytes.set buffer 0 (char_of_int dst_addr_len);
    Bytes.blit dst_addr_buf 0 buffer 1 dst_addr_len;
    Bytes.set_int16_ne buffer (1 + dst_addr_len) dst_port;
    Bytes.blit_string packet 0 buffer packet_offset packet_len;

    send sock buffer;

    let buffer = receive sock in
    print_endline "reply:";
    print_endline (Bytes.to_string buffer)
end

module Msg = struct
  type t = {
    ver : int;
    cmd_rep : int;
    atyp : int;
    addr : string;
    addr_len : int;
    port : int;
  }

  let serialize msg =
    let msg_len = 4 + msg.addr_len + 2 in
    let buffer = Bytes.create msg_len in
    Bytes.set buffer 0 (char_of_int msg.ver);
    Bytes.set buffer 1 (char_of_int msg.cmd_rep);
    Bytes.set buffer 2 '\000';
    Bytes.set buffer 3 (char_of_int msg.atyp);
    Bytes.blit (ipv4_addr_to_bytes msg.addr) 0 buffer 4 msg.addr_len;
    Bytes.set_int16_ne buffer (4 + msg.addr_len) msg.port;
    buffer

  let deserialize buffer =
    let ver = int_of_char (Bytes.get buffer 0) in
    let cmd = int_of_char (Bytes.get buffer 1) in
    let atyp = int_of_char (Bytes.get buffer 3) in
    (*(* 0x1: connect *)
      assert (cmd = 1);*)
    (* 0x1: ipv4 *)
    assert (atyp = 1);
    let addr_len = 4 in
    let addr_buf = Bytes.create addr_len in
    Bytes.blit buffer 4 addr_buf 0 addr_len;
    let addr = ipv4_addr_of_bytes addr_buf in
    let port = Bytes.get_int16_ne buffer (4 + addr_len) in
    Printf.printf "== msg.addr: %s\n" addr;
    Printf.printf "== msg.port: %d\n" port;
    { ver; cmd_rep = cmd; atyp; addr; addr_len; port }
end

let client () =
  print_endline "socks5 client starts...";
  let server_name = gethostname () and port_number = 5678 in
  let server_addr =
    try (gethostbyname server_name).h_addr_list.(0)
    with Not_found ->
      prerr_endline (server_name ^ ": Host not found");
      exit 2
  in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET (server_addr, port_number));
  (*match fork () with
    | 0 ->*)
  let version = 5 in
  let nmethods = 1 in
  let methods = 0x00 in
  let buffer = Bytes.create 3 in
  Bytes.set buffer 0 (char_of_int version);
  Bytes.set buffer 1 (char_of_int nmethods);
  Bytes.set buffer 2 (char_of_int methods);
  let msg_len = 3 in
  let n = write sock buffer 0 msg_len in
  Printf.printf "[sent %4d bytes] " n;
  print_bytes (Bytes.sub buffer 0 n);
  (* exit 0
     | _ ->*)
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let n = read sock buffer 0 buffer_size in
  Printf.printf "[recv %4d bytes] " n;
  print_bytes (Bytes.sub buffer 0 n);

  let listen_addr =
    let listen_addr = (gethostbyname (gethostname ())).h_addr_list.(0) in
    let listen_port = 4567 in
    ADDR_INET (listen_addr, listen_port)
  in
  let listen_sock = Misc.install_tcp_server_socket listen_addr in
  let listen_sock, _ = accept listen_sock in

  let loop () =
    let buffer = receive listen_sock in
    let dst_addr_len = int_of_char (Bytes.get buffer 0) in
    let dst_addr_buf = Bytes.create dst_addr_len in
    Bytes.blit buffer 1 dst_addr_buf 0 dst_addr_len;
    let dst_addr = ipv4_addr_of_bytes dst_addr_buf in
    let dst_port = Bytes.get_int16_ne buffer (1 + dst_addr_len) in
    Printf.printf "== dst addr: %s\n" dst_addr;
    Printf.printf "== dst port: %d\n" dst_port;
    let packet_offset = 1 + dst_addr_len + 2 in
    let packet_len = Bytes.length buffer - packet_offset in
    let packet = Bytes.sub buffer packet_offset packet_len in
    Printf.printf "== packet: %s\n" (Bytes.to_string packet);

    let msg : Msg.t =
      {
        ver = 5;
        cmd_rep = 1;
        atyp = 1;
        addr = dst_addr;
        port = dst_port;
        addr_len = dst_addr_len;
      }
    in
    let buffer = Msg.serialize msg in
    let n = write sock buffer 0 (Bytes.length buffer) in
    Printf.printf "[sent %4d bytes] " n;
    print_bytes (Bytes.sub buffer 0 n);

    let buffer = receive sock in
    let _msg = Msg.deserialize buffer in

    print_endline "connection established, will send data";
    send sock packet;
    print_endline "packet sent...";
    let buffer = receive sock in
    print_endline "received reply:";
    print_endline (Bytes.to_string buffer);
    send listen_sock buffer;
    print_endline "sent reply"
  in
  loop ();
  shutdown sock SHUTDOWN_ALL

let server () =
  print_endline "socks5 server starts...";
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: server <port>";
    exit 2 );
  let port = int_of_string Sys.argv.(1) in
  let host = (gethostbyname (gethostname ())).h_addr_list.(0) in
  let addr = ADDR_INET (host, port) in
  let treat sock ((_, client_addr) as client) =
    ( match client_addr with
    | ADDR_INET (caller, _) ->
        prerr_endline ("Connection from " ^ string_of_inet_addr caller)
    | ADDR_UNIX _ -> prerr_endline "Connection from the Unix domain (???)" );
    let service (s, _) =
      print_endline "[service] entering...";
      let buffer_size = 4096 in
      let buffer = Bytes.create buffer_size in
      let n = read s buffer 0 buffer_size in
      Printf.printf "[recv %4d bytes] " n;
      print_bytes (Bytes.sub buffer 0 n);
      let version = int_of_char (Bytes.get buffer 0) in
      let nmethod = int_of_char (Bytes.get buffer 1) in
      let methods = int_of_char (Bytes.get buffer 2) in
      let buffer = Bytes.create 2 in
      Bytes.set buffer 0 (char_of_int version);
      Bytes.set buffer 1 (char_of_int methods);
      let n = write s buffer 0 2 in
      Printf.printf "[sent %4d bytes] " n;
      print_bytes (Bytes.sub buffer 0 n);
      assert (nmethod = 1 && methods = 0x00);
      (* 0x00: no authentication required *)
      let loop () =
        let buffer_size = 4096 in
        let buffer = Bytes.create buffer_size in
        let n = read s buffer 0 buffer_size in
        Printf.printf "[recv %4d bytes] " n;
        print_bytes (Bytes.sub buffer 0 n);
        let msg = Msg.deserialize buffer in
        (* 0x1: connect *)
        assert (msg.cmd_rep = 1);
        (* 0x1: ipv4 *)
        assert (msg.atyp = 1);

        let dst_inet_addr = (gethostbyname msg.addr).h_addr_list.(0) in
        let dst_sock = socket PF_INET SOCK_STREAM 0 in
        let bnd_addr = "127.0.0.1" in
        let bnd_port = 5679 in
        let msg : Msg.t =
          {
            ver = 5;
            cmd_rep =
              ( try
                  connect dst_sock (ADDR_INET (dst_inet_addr, msg.port));
                  Printf.printf "connection to %s:%d established...\n" msg.addr
                    msg.port;
                  0x00
                with Unix.Unix_error (err, _, _) ->
                  Printf.printf "### %s\n" (error_message err);
                  0x05 );
            atyp = 0x01;
            addr = bnd_addr;
            port = bnd_port;
            addr_len = 4;
          }
        in
        let buffer = Msg.serialize msg in
        let n = write s buffer 0 (Bytes.length buffer) in
        Printf.printf "[sent %4d bytes] " n;
        print_bytes (Bytes.sub buffer 0 n);
        if msg.cmd_rep != 0x00 then (
          shutdown s SHUTDOWN_ALL;
          Printf.printf "closed client connection\n" );

        print_endline "will forward packets...";
        let packet = receive s in
        print_endline "received packet:";
        print_endline (Bytes.to_string packet);
        send dst_sock packet;
        print_endline "sent packet";
        let buffer = receive dst_sock in
        print_endline "received reply from dst";
        send s buffer;
        print_endline "forwarded reply"
      in
      loop ()
    in
    Misc.double_fork_treatment sock service client
  in
  Misc.tcp_server treat addr
