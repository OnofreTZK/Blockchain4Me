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

  val hashing : string -> string -> string

  val proof_of_work : Block.t -> (int * string)

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

  (* Hashing *)
  let hashing str nonce =
    Sha256.to_hex (Sha256.string (nonce ^ str))

  (* mine function *)
  let proof_of_work new_block =
    let fingerprint = (* Correct version -> compare prev block nonce to found current block golden nonce *)
      let aux = ""
      in
      List.map 
          (fun tx -> (Block.tx_to_string tx) ^ aux) (* function *)
          (Block.get_tx_list new_block) (* list *)
          |> fun _ -> aux (* pipe the new list and return the super string *)
    in
    let rec aux nonce =
      let hash = hashing fingerprint (Int32.to_string nonce)
      in
      if (String.compare (String.sub hash 0 target) generate_target) = 0 then
        Printf.printf "Hash: %s | Golden Nonce: %s\n%!" hash (Int32.to_string nonce) (* Golden Nonce found! *)
        |> fun () -> ((Int32.to_int nonce), hash)
      else Printf.printf "Hash: %s | Nonce: %s\n%!" hash (Int32.to_string nonce)
           |> fun () -> aux (Random.int32 (Int32.of_int bound32int))
    in
    aux (Random.int32 (Int32.of_int bound32int))

end
