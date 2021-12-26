open Mirage_crypto_pk

let () = Mirage_crypto_rng_unix.initialize ()

let private_key = Dsa.generate `Fips1024

let public_key = Dsa.pub_of_priv private_key

let str = Z.to_string public_key.y
let str2 = Z.to_string private_key.x

let str_priv_key = Sexplib0.Sexp.to_string (Dsa.sexp_of_priv private_key)

let encoded_priv_key = Base64.encode_string str_priv_key 

(*let str_private_key = Cstruct.to_string (Cstruct.create 89)*)

let () = Printf.printf "Private String: %s\n%!" encoded_priv_key

let signature = Dsa.sign ~key:private_key (Cstruct.of_string "Message to sign")

let encoded_signature = 
  let signature_str = signature |> fun (r, s) -> (Cstruct.to_string r) ^ " " ^ (Cstruct.to_string s)
  in
  Base64.encode_string signature_str

let () = Printf.printf "Signature string: %s\n%!" encoded_signature

let () = Base64.decode_exn encoded_priv_key |> Sexplib0.Sexp_conv.sexp_of_string 
         |> Dsa.priv_of_sexp |> fun _ -> ()






