open Mirage_crypto_pk

let () = Mirage_crypto_rng_unix.initialize ()

let private_key = Dsa.generate `Fips1024

let public_key = Dsa.pub_of_priv private_key

let str = Z.to_string public_key.y
let str2 = Z.to_string private_key.x

let str_priv_key = Sexplib0.Sexp.to_string_hum ~indent:64 (Dsa.sexp_of_priv private_key)

let encoded_priv_key = Base64.encode_string str_priv_key 

(* Current problem -> How to represent the keys with a string? *)
(*let str_private_key = Cstruct.to_string (Cstruct.create 89)*)

let () = Printf.printf "Public string: %s\nPrivate String: %s\n%!" str encoded_priv_key


