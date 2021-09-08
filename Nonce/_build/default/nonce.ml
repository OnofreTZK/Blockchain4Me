(* Nonce mineration algorithm introduction *)

(* user inputs *)
let size_input return_int = Scanf.scanf "%d\n" return_int;;

(* Generates a string with a number of zeros defined as the target *)
let generate_target size =
  let rec aux acc count = 
    if count = size then acc 
    else aux ("0" ^ acc) (count+1)
  in aux "" 0
;;


(* Hashing *)
let hashing str nonce =
  Sha256.to_hex (Sha256.string (nonce ^ str))
;;

(* main function *)
let mine () = 
  let return_int sz =
    if sz < 1 then Printf.printf "Please give number larger than 0\n" |> fun () -> exit 0
    else sz 
  in
  let size = Printf.printf "Number of zeros target:\n%!" |> fun () -> size_input return_int
  in
  let input = Printf.printf "String to hash:\n%!" |> fun () -> read_line ()
  in
  let bound32int = 2147483647
  in
  let rec aux nonce =
    let hash = hashing input (Int32.to_string nonce)
    in
    if (String.compare (String.sub hash 0 size) (generate_target size)) = 0 then 
      Printf.printf "Hash: %s | Golden Nonce: %s\n%!" hash (Int32.to_string nonce) (* Golden Nonce found! *)
    else Printf.printf "Hash: %s | Nonce: %s\n%!" hash (Int32.to_string nonce)
         |> fun () -> aux (Random.int32 (Int32.of_int bound32int)) in (* Add that nonce to the table and keep going *)
  aux (Random.int32 (Int32.of_int bound32int))
;;

let () = mine ();;
