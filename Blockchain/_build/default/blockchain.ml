open Block
open Chain
    
(* Initializing chain with genesis block *)
let seedchain =
  let genesis = Block.create ~nonce:0 ~transactions:[] 
      ~prev_hash:"0" ~hash:(String.init 64 (fun _ -> '0'))
  in
  Blockchain.init genesis

(* GET mine_block *)
let mine_block =
  Blockchain.get_previous_block seedchain

(* GET get_chain *)



