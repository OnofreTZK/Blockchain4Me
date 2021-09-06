let hello_str = "hello world";;

let str_to_ascii str = 
  let rec aux acc count ch = 
    if count = (String.length str) - 1 then acc
    else aux ((Char.code ch) :: acc) (count+1) (String.get str (count+1))
  in
  aux [] 0 (String.get str 0)
;;

let () = Printf.printf "%s\n" (Sha256.to_hex (Sha256.string hello_str));;


