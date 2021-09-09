(* Serializable record *)
type ledger = {
    mutable block_index : int;
    timestamp : string;
    nonce : int;
    transactions : list;
    prev_hash : string;
    hash : string;
  }[@@deriving yojson]
