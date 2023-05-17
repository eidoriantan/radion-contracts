
type admin_storage = {
  admin : address;
  pending_admin : address option;
  paused : bool;
}
type admin_entrypoints =
  | Set_admin of address
  | Confirm_admin of unit
  | Pause of bool

let confirm_new_admin (storage : admin_storage) : admin_storage =
  match storage.pending_admin with
  | None -> (failwith "NO_PENDING_ADMIN" : admin_storage)
  | Some pending ->
    if Tezos.sender = pending
    then { storage with
      pending_admin = (None : address option);
      admin = Tezos.sender;
    }
    else (failwith "NOT_A_PENDING_ADMIN" : admin_storage)

(* Fails if sender is not admin *)
let fail_if_not_admin_ext (storage, extra_msg : admin_storage * string) : unit =
  if Tezos.sender <> storage.admin
  then failwith ("NOT_AN_ADMIN" ^  " "  ^ extra_msg)
  else unit

(* Fails if sender is not admin *)
let fail_if_not_admin (storage : admin_storage) : unit =
  if Tezos.sender <> storage.admin
  then failwith "NOT_AN_ADMIN"
  else unit

(* Returns true if sender is admin *)
let is_admin (storage : admin_storage) : bool = Tezos.sender = storage.admin

let fail_if_paused (storage : admin_storage) : unit =
  if(storage.paused)
  then failwith "PAUSED"
  else unit

(*Only callable by admin*)
let set_admin (new_admin, storage : address * admin_storage) : admin_storage =
  let u = fail_if_not_admin storage in
  { storage with pending_admin = Some new_admin; }

(*Only callable by admin*)
let pause (paused, storage: bool * admin_storage) : admin_storage =
  let u = fail_if_not_admin storage in
  { storage with paused = paused; }

let admin_main(param, storage : admin_entrypoints * admin_storage)
    : (operation list) * admin_storage =
  match param with
  | Set_admin new_admin ->
      let new_s = set_admin (new_admin, storage) in
      (([] : operation list), new_s)

  | Confirm_admin u ->
      let new_s = confirm_new_admin storage in
      (([]: operation list), new_s)

  | Pause paused ->
      let new_s = pause (paused, storage) in
      (([]: operation list), new_s)

type token_id = nat

type transfer_destination =
[@layout:comb]
{
  to_ : address;
  token_id : token_id;
  amount : nat;
}

type transfer =
[@layout:comb]
{
  from_ : address;
  txs : transfer_destination list;
}

type balance_of_request =
[@layout:comb]
{
  owner : address;
  token_id : token_id;
}

type balance_of_response =
[@layout:comb]
{
  request : balance_of_request;
  balance : nat;
}

type balance_of_param =
[@layout:comb]
{
  requests : balance_of_request list;
  callback : (balance_of_response list) contract;
}

type operator_param =
[@layout:comb]
{
  owner : address;
  operator : address;
  token_id: token_id;
}

type update_operator =
[@layout:comb]
  | Add_operator of operator_param
  | Remove_operator of operator_param

type token_metadata =
[@layout:comb]
  {
    token_id: token_id;
    token_info: ((string, bytes) map);
  }

type token_metadata_param =
[@layout:comb]
{
  token_ids : token_id list;
  handler : (token_metadata list) -> unit;
}

type token_metadata_storage = (token_id, token_metadata) big_map

type fa2_entry_points =
  | Transfer of transfer list
  | Balance_of of balance_of_param
  | Update_operators of update_operator list

type fa2_token_metadata =
  | Token_metadata of token_metadata_param

(* permission policy definition *)

type operator_transfer_policy =
  [@layout:comb]
  | No_transfer
  | Owner_transfer
  | Owner_or_operator_transfer

type owner_hook_policy =
  [@layout:comb]
  | Owner_no_hook
  | Optional_owner_hook
  | Required_owner_hook

type custom_permission_policy =
[@layout:comb]
{
  tag : string;
  config_api: address option;
}

type permissions_descriptor =
[@layout:comb]
{
  operator : operator_transfer_policy;
  receiver : owner_hook_policy;
  sender : owner_hook_policy;
  custom : custom_permission_policy option;
}

type transfer_destination_descriptor =
[@layout:comb]
{
  to_ : address option;
  token_id : token_id;
  amount : nat;
}

type transfer_descriptor =
[@layout:comb]
{
  from_ : address option;
  txs : transfer_destination_descriptor list
}

type transfer_descriptor_param =
[@layout:comb]
{
  batch : transfer_descriptor list;
  operator : address;
}

let fa2_token_undefined = "FA2_TOKEN_UNDEFINED"
let fa2_insufficient_balance = "FA2_INSUFFICIENT_BALANCE"
let fa2_tx_denied = "FA2_TX_DENIED"
let fa2_not_owner = "FA2_NOT_OWNER"
let fa2_not_operator = "FA2_NOT_OPERATOR"
let fa2_operators_not_supported = "FA2_OPERATORS_UNSUPPORTED"
let fa2_receiver_hook_failed = "FA2_RECEIVER_HOOK_FAILED"
let fa2_sender_hook_failed = "FA2_SENDER_HOOK_FAILED"
let fa2_receiver_hook_undefined = "FA2_RECEIVER_HOOK_UNDEFINED"
let fa2_sender_hook_undefined = "FA2_SENDER_HOOK_UNDEFINED"

type operator_storage = ((address * (address * token_id)), unit) big_map

(**
  Updates operator storage using an `update_operator` command.
  Helper function to implement `Update_operators` FA2 entrypoint
*)
let update_operators (update, storage : update_operator * operator_storage)
    : operator_storage =
  match update with
  | Add_operator op ->
    Big_map.update (op.owner, (op.operator, op.token_id)) (Some unit) storage
  | Remove_operator op ->
    Big_map.remove (op.owner, (op.operator, op.token_id)) storage

(**
Validate if operator update is performed by the token owner.
@param updater an address that initiated the operation; usually `Tezos.sender`.
*)
let validate_update_operators_by_owner (update, updater : update_operator * address)
    : unit =
  let op = match update with
  | Add_operator op -> op
  | Remove_operator op -> op
  in
  if op.owner = updater then unit else failwith fa2_not_owner

(**
  Generic implementation of the FA2 `%update_operators` entrypoint.
  Assumes that only the token owner can change its operators.
 *)
let fa2_update_operators (updates, storage
    : (update_operator list) * operator_storage) : operator_storage =
  let updater = Tezos.sender in
  let process_update = (fun (ops, update : operator_storage * update_operator) ->
    let u = validate_update_operators_by_owner (update, updater) in
    update_operators (update, ops)
  ) in
  List.fold process_update updates storage

(**
  owner * operator * token_id * ops_storage -> unit
*)

type operator_params = address * address * token_id * operator_storage

type operator_validator = operator_params -> unit

(**
Default implementation of the operator validation function.
The default implicit `operator_transfer_policy` value is `Owner_or_operator_transfer`
 *)
let default_operator_validator : operator_validator =
  (fun
    (owner, operator, token_id, ops_storage : operator_params) ->
    if owner = operator
    then unit (* transfer by the owner *)
    else if Big_map.mem (owner, (operator, token_id)) ops_storage
    then unit (* the operator is permitted for the token_id *)
    else failwith fa2_not_operator (* the operator is not permitted for the token_id *)
  )

(**
Create an operator validator function based on provided operator policy.
@param tx_policy operator_transfer_policy defining the constrains on who can transfer.
@return (owner, operator, token_id, ops_storage) -> unit
 *)

let make_operator_validator (tx_policy : operator_transfer_policy) : operator_validator =
  let can_owner_tx, can_operator_tx = match tx_policy with
  | No_transfer -> (failwith fa2_tx_denied : bool * bool)
  | Owner_transfer -> true, false
  | Owner_or_operator_transfer -> true, true
  in
  (fun (owner, operator, token_id, ops_storage
      : address * address * token_id * operator_storage) ->
    if can_owner_tx && owner = operator
    then unit (* transfer by the owner *)
    else if not can_operator_tx
    then failwith fa2_not_owner (* an operator transfer not permitted by the policy *)
    else if Big_map.mem  (owner, (operator, token_id)) ops_storage
    then unit (* the operator is permitted for the token_id *)
    else failwith fa2_not_operator (* the operator is not permitted for the token_id *)
  )
(**
Validate operators for all transfers in the batch at once
@param tx_policy operator_transfer_policy defining the constrains on who can transfer.
*)
let validate_operator (tx_policy, txs, ops_storage
    : operator_transfer_policy * (transfer list) * operator_storage) : unit =
  let validator = make_operator_validator tx_policy in
  List.iter (fun (tx : transfer) ->
    List.iter (fun (dst: transfer_destination) ->
      validator (tx.from_, Tezos.sender, dst.token_id ,ops_storage)
    ) tx.txs
  ) txs

(* (owner,token_id) -> balance *)
type ledger = ((address * token_id), nat) big_map

(* token_id -> total_supply *)
type token_total_supply = (token_id, nat) big_map

type multi_nft_token_storage = {
  ledger : ledger;
  operators : operator_storage;
  token_total_supply : token_total_supply;
  token_metadata : token_metadata_storage;
  next_token_id : token_id;
}

let get_balance_amt (key, ledger : (address * nat) * ledger) : nat =
  let bal_opt = Big_map.find_opt key ledger in
  match bal_opt with
  | None -> 0n
  | Some b -> b

let inc_balance (owner, token_id, amt, ledger
    : address * token_id * nat * ledger) : ledger =
  let key = owner, token_id in
  let bal = get_balance_amt (key, ledger) in
  let updated_bal = bal + amt in
  if updated_bal = 0n
  then Big_map.remove key ledger
  else Big_map.update key (Some updated_bal) ledger

let dec_balance (owner, token_id, amt, ledger
    : address * token_id * nat * ledger) : ledger =
  let key = owner, token_id in
  let bal = get_balance_amt (key, ledger) in
  match Michelson.is_nat (bal - amt) with
  | None -> (failwith fa2_insufficient_balance : ledger)
  | Some new_bal ->
    if new_bal = 0n
    then Big_map.remove key ledger
    else Big_map.update key (Some new_bal) ledger

(**
Update leger balances according to the specified transfers. Fails if any of the
permissions or constraints are violated.
@param txs transfers to be applied to the ledger
@param validate_op function that validates of the tokens from the particular owner can be transferred.
 *)
let transfer (txs, validate_op, storage
    : (transfer list) * operator_validator * multi_nft_token_storage)
    : ledger =
  let make_transfer = fun (l, tx : ledger * transfer) ->
    List.fold
      (fun (ll, dst : ledger * transfer_destination) ->
        if not Big_map.mem dst.token_id storage.token_metadata
        then (failwith fa2_token_undefined : ledger)
        else
          let u : unit = validate_op (tx.from_, Tezos.sender, dst.token_id, storage.operators) in
          let lll = dec_balance (tx.from_, dst.token_id, dst.amount, ll) in
          inc_balance(dst.to_, dst.token_id, dst.amount, lll)
      ) tx.txs l
  in
  List.fold make_transfer txs storage.ledger

let get_balance (p, ledger, tokens
    : balance_of_param * ledger * token_metadata_storage) : operation =
  let to_balance = fun (r : balance_of_request) ->
    if not Big_map.mem r.token_id tokens
    then (failwith fa2_token_undefined : balance_of_response)
    else
      let key = r.owner, r.token_id in
      let bal = get_balance_amt (key, ledger) in
      let response : balance_of_response = { request = r; balance = bal; } in
      response
  in
  let responses = List.map to_balance p.requests in
  Tezos.transaction responses 0mutez p.callback

let fa2_main (param, storage : fa2_entry_points * multi_nft_token_storage)
    : (operation  list) * multi_nft_token_storage =
  match param with
  | Transfer txs ->
    let new_ledger = transfer (txs, default_operator_validator, storage) in
    let new_storage = { storage with ledger = new_ledger; }
    in ([] : operation list), new_storage

  | Balance_of p ->
    let op = get_balance (p, storage.ledger, storage.token_metadata) in
    [op], storage

  | Update_operators updates ->
    let new_ops = fa2_update_operators (updates, storage.operators) in
    let new_storage = { storage with operators = new_ops; } in
    ([] : operation list), new_storage

type mint_burn_tx =
[@layout:comb]
{
  owner : address;
  token_id : token_id;
  amount : nat;
}

type mint_burn_tokens_param = mint_burn_tx list


(* `token_manager` entry points *)
type token_manager =
  | Create_token of token_metadata
  | Mint_tokens of mint_burn_tokens_param
  | Burn_tokens of mint_burn_tokens_param


let create_token (metadata, storage
    : token_metadata * multi_nft_token_storage) : multi_nft_token_storage =
  (* extract token id *)
  let new_token_id = metadata.token_id in
  let existing_meta = Big_map.find_opt new_token_id storage.token_metadata in
  match existing_meta with
  | Some m -> (failwith "FA2_DUP_TOKEN_ID" : multi_nft_token_storage)
  | None ->
    let meta = Big_map.add new_token_id metadata storage.token_metadata in
    let supply = Big_map.add new_token_id 0n storage.token_total_supply in
    { storage with
      token_metadata = meta;
      token_total_supply = supply;
    }


let  mint_update_balances (txs, ledger : (mint_burn_tx list) * ledger) : ledger =
  let mint = fun (l, tx : ledger * mint_burn_tx) ->
    inc_balance (tx.owner, tx.token_id, tx.amount, l) in

  List.fold mint txs ledger

let mint_update_total_supply (txs, total_supplies
    : (mint_burn_tx list) * token_total_supply) : token_total_supply =
  let update = fun (supplies, tx : token_total_supply * mint_burn_tx) ->
    let supply_opt = Big_map.find_opt tx.token_id supplies in
    match supply_opt with
    | None -> (failwith fa2_token_undefined : token_total_supply)
    | Some ts ->
      let new_s = ts + tx.amount in
      Big_map.update tx.token_id (Some new_s) supplies in

  List.fold update txs total_supplies

let mint_tokens (param, storage : mint_burn_tokens_param * multi_nft_token_storage)
    : multi_nft_token_storage =
    let new_ledger = mint_update_balances (param, storage.ledger) in
    let new_supply = mint_update_total_supply (param, storage.token_total_supply) in
    let new_s = { storage with
      ledger = new_ledger;
      token_total_supply = new_supply;
    } in
    new_s

let burn_update_balances(txs, ledger : (mint_burn_tx list) * ledger) : ledger =
  let burn = fun (l, tx : ledger * mint_burn_tx) ->
    dec_balance (tx.owner, tx.token_id, tx.amount, l) in

  List.fold burn txs ledger

let burn_update_total_supply (txs, total_supplies
    : (mint_burn_tx list) * token_total_supply) : token_total_supply =
  let update = fun (supplies, tx : token_total_supply * mint_burn_tx) ->
    let supply_opt = Big_map.find_opt tx.token_id supplies in
    match supply_opt with
    | None -> (failwith fa2_token_undefined : token_total_supply)
    | Some ts ->
      let new_s = match Michelson.is_nat (ts - tx.amount) with
      | None -> (failwith fa2_insufficient_balance : nat)
      | Some s -> s
      in
      Big_map.update tx.token_id (Some new_s) supplies in

  List.fold update txs total_supplies

let burn_tokens (param, storage : mint_burn_tokens_param * multi_nft_token_storage)
    : multi_nft_token_storage =

    let new_ledger = burn_update_balances (param, storage.ledger) in
    let new_supply = burn_update_total_supply (param, storage.token_total_supply) in
    let new_s = { storage with
      ledger = new_ledger;
      token_total_supply = new_supply;
    } in
    new_s

let ft_token_manager (param, s : token_manager * multi_nft_token_storage)
    : (operation list) * multi_nft_token_storage =
  match param with

  | Create_token token_metadata ->
    let new_s = create_token (token_metadata, s) in
    (([]: operation list), new_s)

  | Mint_tokens param ->
    let new_s = mint_tokens (param, s) in
    ([] : operation list), new_s

  | Burn_tokens param ->
    let new_s = burn_tokens (param, s) in
    ([] : operation list), new_s

type mint_fixed_supply_token_param =
  [@layout:comb]
  {
    owner : address;
    amount : nat;
    token_info : ((string, bytes) map);
  }

type mint_fixed_supply_tokens_param = mint_fixed_supply_token_param list

(* `token_manager` entry points *)
type token_manager =
  | Mint of mint_fixed_supply_tokens_param

type create_tokens_accumulator =
  [@layout:comb]
  {
      new_tokens_metadata : token_metadata list;
      mint_tokens_param : mint_burn_tokens_param;
      next_token_id : token_id;
  }

let mint_fixed_supply_tokens (params, storage :
  mint_fixed_supply_tokens_param * multi_nft_token_storage) : multi_nft_token_storage =
  let { new_tokens_metadata = new_tokens_metadata;
        mint_tokens_param = mint_tokens_param;
        next_token_id = next_token_id;
      } : create_tokens_accumulator =
    (List.fold
      (fun (acc, param : create_tokens_accumulator * mint_fixed_supply_token_param) ->
        let md : token_metadata = {
            token_id = acc.next_token_id;
            token_info = param.token_info;
        }  in
        let mint_token_param : mint_burn_tx = {
            owner = param.owner;
            amount = param.amount;
            token_id = acc.next_token_id;
        } in
        let next_token_id : token_id = acc.next_token_id + 1n in

        ({new_tokens_metadata = md :: acc.new_tokens_metadata;
         mint_tokens_param = mint_token_param :: acc.mint_tokens_param;
         next_token_id = next_token_id;} : create_tokens_accumulator)
      )
      params
      ({
          new_tokens_metadata = ([] : token_metadata list);
          mint_tokens_param = ([] : mint_burn_tokens_param);
          next_token_id = storage.next_token_id;
      } : create_tokens_accumulator)
    ) in
  let new_s : multi_nft_token_storage =
    (List.fold_right create_token new_tokens_metadata storage) in
  let new_s = mint_tokens(mint_tokens_param, new_s) in
  {new_s with next_token_id = next_token_id}


let ft_token_manager (param, s : token_manager * multi_nft_token_storage)
    : (operation list) * multi_nft_token_storage =
  match param with

    Mint tokens ->
      let new_s = mint_fixed_supply_tokens (tokens, s) in
      ([] : operation list), new_s

type multi_nft_asset_storage = {
  admin : admin_storage;
  assets : multi_nft_token_storage;
  metadata : (string, bytes) big_map;
}

type set_metadata_param =
[@layout:comb]
{
  name: string;
  metadata: bytes;
}

type multi_nft_asset_param =
  | Assets of fa2_entry_points
  | Admin of admin_entrypoints
  | [@annot:mint] Tokens of token_manager
  | Set_metadata of set_metadata_param

let set_metadata (param, storage : set_metadata_param * multi_nft_asset_storage) : (operation list) * multi_nft_asset_storage =
  let new_s = { storage with metadata = Big_map.update param.name (Some param.metadata) storage.metadata } in
  ([] : operation list), new_s

let multi_nft_asset_main
    (param, s : multi_nft_asset_param * multi_nft_asset_storage)
    : (operation list) * multi_nft_asset_storage =
  match param with
  | Admin p ->
      let ops, admin = admin_main (p, s.admin) in
      let new_s = { s with admin = admin; } in
      (ops, new_s)

  | Tokens p ->
      let ops, assets = ft_token_manager (p, s.assets) in
      let new_s = { s with
        assets = assets
      } in
      (ops, new_s)

  | Assets p ->
      let _u : unit = fail_if_paused s.admin in
      let ops, assets = fa2_main (p, s.assets) in
      let new_s = { s with assets = assets } in
      (ops, new_s)

  | Set_metadata p ->
      let _u : unit = fail_if_not_admin s.admin in
      set_metadata(p, s)
