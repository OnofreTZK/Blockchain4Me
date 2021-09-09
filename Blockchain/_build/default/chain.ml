open Block

(* Block type *)
module Blockchain : sig

  type t

  val add_block : Block.t -> unit

end = struct

  type t = Block.t list

  let add_block block = 
    block :: t |> fun _ -> ()

end
