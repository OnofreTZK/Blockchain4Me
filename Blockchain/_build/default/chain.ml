open Block

(* For future ->
 * Use module type to write a module interface and
 * module ModuleImpl : ModuleType to write the struct in another file *)

(* Chain type *)
module Blockchain : sig

  type t

  val target : int

  val bound32int : int

  val init : Block.t -> t
 
  val add_block : Block.t -> t -> unit

  val get_previous_block : t -> Block.t

  val generate_target : string

  val hash_of_nonce : string -> string -> string

  val hash_of_string : string -> string

  val proof_of_work_test_v : Block.t -> (int * string)

  val proof_of_work : int -> (int * string)

  val chain_is_valid  : t -> bool

end = struct

  type t = Block.t list

  (* Target of zeros *)
  let target = 4

  (* Range for random library *)
  let bound32int = 2147483647

  (* Initialize the chain with genesis block *)
  let init genesis = [genesis]
  
  (* Add new block to the chain *)
  let add_block block chain =
    let idx = if (List.length chain) = 0 then 0 else (List.length chain - 1)
    in
    Block.insert_index block idx
    |> fun () -> block :: chain 
    |> fun _ -> ()

  (* Returns the last block *)
  let get_previous_block chain =
    List.nth chain ((List.length chain) - 1)

  (* Generates a string with a number of zeros defined as the target *)
  let generate_target =
    let rec aux acc count = 
      if count = target then acc 
      else aux ("0" ^ acc) (count+1)
    in aux "" 0

  (* Hashing with nonce concatened *)
  let hash_of_nonce str nonce =
    Sha256.to_hex (Sha256.string (nonce ^ str))

  (* Get hash from block json *)
  let hash_of_string str =
    Sha256.to_hex (Sha256.string str)

  (* mine function (wrong version)*)
  let proof_of_work_test_v new_block =
    let fingerprint = (* Correct version -> compare prev block nonce to found current block golden nonce *)
      let aux = ""
      in
      List.map 
          (fun tx -> (Block.tx_to_string tx) ^ aux) (* function *)
          (Block.get_tx_list new_block) (* list *)
          |> fun _ -> aux (* pipe the new list and return the super string *)
    in
    let rec aux nonce =
      let hash = hash_of_nonce fingerprint (Int32.to_string nonce)
      in
      if (String.compare (String.sub hash 0 target) generate_target) = 0 then
        Printf.printf "Hash: %s | Golden Nonce: %s\n%!" hash (Int32.to_string nonce) (* Golden Nonce found! *)
        |> fun () -> ((Int32.to_int nonce), hash)
      else Printf.printf "Hash: %s | Nonce: %s\n%!" hash (Int32.to_string nonce)
           |> fun () -> aux (Random.int32 (Int32.of_int bound32int))
    in
    aux (Random.int32 (Int32.of_int bound32int))
  
  (* mine function (correct nonce version)*)
  let proof_of_work prev_nonce =
    let rec pow a = function
      | 0 -> 1
      | 1 -> a
      | n -> pow a (n/2)
    in
    let fingerprint current_nonce = (* Correct version -> compare prev block nonce to found current block golden nonce *)
      Int.to_string ((pow (Int32.to_int current_nonce) 2) + (pow prev_nonce 2))  
    in
    let rec aux nonce =
      let hash = hash_of_string (fingerprint nonce)
      in
      if (String.compare (String.sub hash 0 target) generate_target) = 0 then
        Printf.printf "Hash: %s | Golden Nonce: %s\n%!" hash (Int32.to_string nonce) (* Golden Nonce found! *)
        |> fun () -> ((Int32.to_int nonce), hash)
      else Printf.printf "Hash: %s | Nonce: %s\n%!" hash (Int32.to_string nonce)
           |> fun () -> aux (Random.int32 (Int32.of_int bound32int))
    in
    aux (Random.int32 (Int32.of_int bound32int))

  (* Chain validation *)
  let chain_is_valid chain =
    let rec aux = function
      | [] | _ :: [] -> true
      | prev :: (curr :: _ as tl) -> if (Block.valid_crypto prev curr) then aux tl
        else false
    in aux chain


end
