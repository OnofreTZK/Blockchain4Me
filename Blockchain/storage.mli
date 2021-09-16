open Block
open Chain

val get_chain : unit -> Blockchain.t Lwt.t

val insert_block : Block.t -> unit Lwt.t
