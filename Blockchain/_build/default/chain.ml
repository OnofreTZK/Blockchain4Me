(* For future ->
 * Use module type to write a module interface a file and
 * module ModuleImpl : ModuleType to write the struct in another file *)

(* Transaction type *)
(* Serializable record *)  
type transaction = {
  from_ : string;
  amount : float;
  to_ : string;
}[@@deriving yojson]

(* Block type *)
(* Serializable record *)
type block = {
  mutable block_index : int;
  timestamp : string;
  nonce : int;
  transactions : transaction list;
  prev_hash : string;
  hash : string;
}[@@deriving yojson]


(* Chain type *)
module Blockchain : sig

  type t

  val chain : t 

  val target : int

  val bound32int : int

  val create_block : 
    nonce:int -> transactions:transaction list -> prev_hash:string -> hash:string -> block
  
  val add_block : block -> unit

  val get_previous_block : unit -> block

  val generate_target : string

  val hashing : string -> string -> string

  val proof_of_work : block -> (int * string)

end = struct

  type t = block list

  let chain = []

  (* Target of zeros *)
  let target = 4

  (* Range for random library *)
  let bound32int = 2147483647

  (* Create a new block after the mining *)
  let create_block ~nonce ~transactions ~prev_hash ~hash =
    let timestamp = Float.to_string (Unix.time ())
    in
    {block_index=0; timestamp=timestamp; nonce=nonce; 
     transactions=transactions; prev_hash=prev_hash; hash=hash}
  
  (* Add new block to the chain *)
  let add_block block =
    let idx = if (List.length chain) = 0 then 0 else (List.length chain - 1)
    in
    block.block_index <- idx;
    block :: chain |> fun _ -> ()

  (* Returns the last block *)
  let get_previous_block () =
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
    let fingerprint =
      let aux = ""
      in
      List.map 
          (fun tx -> (tx.from_ ^ (Float.to_string tx.amount) ^ tx.to_) ^ aux) (* function *)
          new_block.transactions (* list *)
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
