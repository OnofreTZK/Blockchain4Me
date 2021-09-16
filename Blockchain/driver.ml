open Block
open Chain
open Transaction
open Opium
    

(* GET mine_block *)
let mine_block req =
  let open Lwt.Syntax 
  in
  let* old_chain = Storage.get_chain ()
  in
  let new_block = 
    Blockchain.mine_block old_chain [{from_="Cleveland"; Transaction.amount=54000000.; to_="Lakers"}]
  in
  Storage.insert_block new_block |> fun _ -> ();
  let open Lwt.Syntax 
  in
  let* new_chain = Storage.get_chain () (*Inserting and retrieving*)
  in
  let updated_block = Blockchain.get_previous_block new_chain
  in
  let json = Block.block_to_yojson updated_block 
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



