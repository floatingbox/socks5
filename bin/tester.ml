let uri = Sys.argv.(1) in
Printf.printf "uri: '%s'\n" uri;
let socks5_addr = Unix.gethostname () in
let socks5_port = 4567 in
Socks5.Tester.http_request uri socks5_addr socks5_port
