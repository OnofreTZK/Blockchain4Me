open Transaction

module Block : sig 

  type t

  val create : nonce:int -> transactions: Transaction.t list -> prev_hash:string -> hash:string -> t

  val insert_index : t -> int -> unit

  val get_tx_list : t -> Transaction.t list

end = struct 

  (* Block type *)
  (* Serializable record *)
  type t = {
    mutable block_index : int;
    timestamp : string;
    nonce : int;
    transactions : Transaction.t list;
    prev_hash : string;
    hash : string;
  }[@@deriving yojson]


  let create ~nonce ~transactions ~prev_hash ~hash =
    let timestamp = Float.to_string (Unix.time ())
    in
    {block_index=0; timestamp=timestamp; nonce=nonce; 
     transactions=transactions; prev_hash=prev_hash; hash=hash}

  let insert_index block idx =
    block.block_index <- idx;
    ()

  let get_tx_list block =
    block.transactions

end

