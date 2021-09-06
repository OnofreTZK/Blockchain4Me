let hello_str = "hello world";;

let () = Printf.printf "%s\n" (Sha256.to_hex (Sha256.string hello_str));;


