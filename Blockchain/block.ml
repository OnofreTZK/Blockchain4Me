open Ledger

(* Block type *)
module Block : sig

  type t

  val create : int -> list -> string -> string -> t

end = struct

  type t = ledger

  let create ~proof ~transactions ~prev_hash ~hash =
    let index = 0 
    in
    let timestamp = Float.to_string Unix.time
    in
    {index; timestamp; proof; transactions; prev_hash; Ledger.hash}
  ;;

end
