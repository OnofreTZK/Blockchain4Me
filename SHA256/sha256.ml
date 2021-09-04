(* Convert to binary *)

let hello_str = "hello world";;

let str_to_ascii str = 
  let rec aux acc count ch = 
    if count = (String.length str) - 1 then acc
    else aux ((Char.code ch) :: acc) (count+1) (String.get str (count+1))
  in
  aux [] 0 (String.get str 0)
;;

let print_list ls =
  let rec aux = function
    | [] -> ()
    | hd :: tl -> Printf.printf "%d\t" hd |> fun () -> aux tl
  in
  aux ls;;

let () = print_list (str_to_ascii hello_str);;


