(* Nonce mineration algorithm introduction *)

(* user inputs *)
let size_input return_int = Scanf.scanf "%d\n" return_int;;
let string_input return_str = Scanf.scanf "%s\n" return_str;;

(* Generates a string with a number of zeros defined as the target *)
let generate_target size =
  let rec aux acc count = 
    if count = size then acc 
    else aux ("0" ^ acc) (count+1)
  in aux "" 0
;;

let () = Printf.printf "%s\n" (generate_target 7);;

