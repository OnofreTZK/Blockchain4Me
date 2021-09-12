open Chain
    
(* Initializing chain with genesis block *)
let seedchain =
  let genesis_timestamp = Float.to_string (Unix.time ()) 
  in
  [{block_index=0; timestamp=genesis_timestamp; nonce=0; 
    transactions=[]; prev_hash="0"; hash=String.init 64 (fun _ -> '0')}]

(* GET mine_block *)

(* GET get_chain *)



