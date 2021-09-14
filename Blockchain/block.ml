open Transaction

module Block : sig 

  type t

  val create : nonce:int -> transactions: Transaction.t list -> prev_hash:string -> hash:string -> t

  val insert_index : t -> int -> unit

  val get_tx_list : t -> Transaction.t list

  val valid_crypto : t -> t -> bool

  val tx_to_string : Transaction.t -> string

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


  (* Return a new block *)
  let create ~nonce ~transactions ~prev_hash ~hash =
    let timestamp = Float.to_string (Unix.time ())
    in
    {block_index=0; timestamp=timestamp; nonce=nonce; 
     transactions=transactions; prev_hash=prev_hash; hash=hash}

  (* Update index with the current position in chain *)
  let insert_index block idx =
    block.block_index <- idx;
    ()

  (* Return the list of transactions of a block *)
  let get_tx_list block =
    block.transactions

  let valid_crypto prev curr =
    String.equal prev.hash curr.prev_hash

  let tx_to_string tx = 
    Transaction.to_string tx

end

