open Mirage_crypto_pk

let () = Mirage_crypto_rng_unix.initialize ()

let private_key = Dsa.generate `Fips2048

let public_key = Dsa.pub_of_priv private_key

let str = Z.to_string public_key.y
let str2 = Z.to_string private_key.x

(* Current problem -> How to represent the keys with a string? *)
(*let str_private_key = Cstruct.to_string (Cstruct.create 89)*)

let () = Printf.printf "Public string: %s\nPrivate String: %s\n%!" str str2


