open Block
open Chain
open Transaction
open Opium
    
(* Initializing chain with genesis block *)
let seedchain =
  let genesis = Block.create ~nonce:0 ~transactions:[] ~prev_hash:(String.init 64 (fun _ -> '0'))
  in
  ref (Blockchain.init genesis) (* Reference to a chain with genesis block *)


(* GET mine_block *)
let mine_block req = 
  let new_block = 
    let block = Blockchain.mine_block !seedchain [{from_="Cleveland"; Transaction.amount=54000000.; to_="Lakers"}]
    in
    seedchain := Blockchain.add_block block !seedchain;
    Blockchain.get_previous_block !seedchain
  in
  let json = Block.block_to_yojson new_block 
  in
  let response = Response.of_json json
  in
  req |> fun _req -> Lwt.return response
  
(* GET get_chain *)


(* App *)
let _ = 
  App.empty
  |> App.get "/blockchain/mine" mine_block
  |> App.run_command 



