open Ledger

(* Block type *)
module Block : sig

  type t

  val create : int -> list -> string -> string -> t

end = struct

  (* Serializable record *)
  type t = {
      mutable block_index : int;
      timestamp : string;
      nonce : int;
      transactions : list;
      prev_hash : string;
      hash : string;
    }[@@deriving yojson]
  
  let create ~proof ~transactions ~prev_hash ~hash =
    let index = 0 
    in
    let timestamp = Float.to_string Unix.time
    in
    {index; timestamp; proof; transactions; prev_hash; Ledger.hash}
  ;;

end
