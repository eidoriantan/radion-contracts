
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

let transfer_fa2(fa2_address, token_id, amount_, from, to_: address * token_id * nat * address * address): operation =
  let fa2_transfer : ((transfer list) contract) option =
      Tezos.get_entrypoint_opt "%transfer"  fa2_address in
  let transfer_op = match fa2_transfer with
  | None -> (failwith "CANNOT_INVOKE_FA2_TRANSFER" : operation)
  | Some c ->
    let tx = {
      from_ = from;
      txs= [{
        to_ = to_;
        token_id = token_id;
        amount = amount_;
    }]} in
    Tezos.transaction [tx] 0mutez c
 in transfer_op

let transfer_nft(fa2_address, token_id, from, to_: address * token_id * address * address): operation =
  let fa2_transfer : ((transfer list) contract) option =
      Tezos.get_entrypoint_opt "%transfer" fa2_address in
  let transfer_op = match fa2_transfer with
  | None -> (failwith "CANNOT_INVOKE_FA2_TRANSFER" : operation)
  | Some c ->
    let tx = {
      from_ = from;
      txs= [{
        to_ = to_;
        token_id = token_id;
        amount = 1n;
    }]} in
    Tezos.transaction [tx] 0mutez c in
  transfer_op

let transfer_tez (qty, to_ : tez * address) : operation =
  let destination = (match (Tezos.get_contract_opt to_ : unit contract option) with
    | None -> (failwith "ADDRESS_DOES_NOT_RESOLVE" : unit contract)
    | Some acc -> acc) in
  Tezos.transaction () qty destination

type pauseable_admin =
  | Set_admin of address
  | Confirm_admin of unit
  | Pause of bool

type pauseable_admin_storage_record = {
  admin : address;
  pending_admin : address option;
  paused : bool;
}

type pauseable_admin_storage = pauseable_admin_storage_record option

let confirm_new_admin (storage : pauseable_admin_storage) : pauseable_admin_storage =
  match storage with
    | Some s ->
        ( match s.pending_admin with
          | None -> (failwith "NO_PENDING_ADMIN" : pauseable_admin_storage)
          | Some pending ->
            if Tezos.sender = pending
            then (Some ({s with
              pending_admin = (None : address option);
              admin = Tezos.sender;
            } : pauseable_admin_storage_record))
            else (failwith "NOT_A_PENDING_ADMIN" : pauseable_admin_storage))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let fail_if_not_admin (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith "NOT_AN_ADMIN"
        else unit
    | None -> unit

let fail_if_not_admin_ext (storage, extra_msg : pauseable_admin_storage * string) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith ("NOT_AN_ADMIN" ^  " "  ^ extra_msg)
        else unit
    | None -> unit

let set_admin (new_admin, storage : address * pauseable_admin_storage) : pauseable_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with pending_admin = Some new_admin; } : pauseable_admin_storage_record))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let pause (paused, storage: bool * pauseable_admin_storage) : pauseable_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with paused = paused; } : pauseable_admin_storage_record ))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let fail_if_paused (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if a.paused
        then failwith "PAUSED"
        else unit
    | None -> unit

let pauseable_admin (param, storage : pauseable_admin *pauseable_admin_storage)
    : (operation list) * (pauseable_admin_storage) =
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

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  funds: nat;
  reward: nat;
  token_address: address;
  token_id: nat;
}

type reward_entrypoints =
  | Reward of address list
  | Add_funds of nat
  | Withdraw_funds of nat
  | Update_reward of nat
  | Admin of pauseable_admin

type rewards_param =
[@layout:comb]
{
  reward: nat;
  token_address: address;
  token_id: token_id;
  ops: operation list;
  total: nat;
}

let process_rewards (rewards_param, receiver : rewards_param * address) : rewards_param =
  let token_address = rewards_param.token_address in
  let token_id = rewards_param.token_id in
  let reward = rewards_param.reward in
  { rewards_param with
    ops = transfer_fa2(token_address, token_id, reward, Tezos.self_address, receiver) :: rewards_param.ops;
    total = rewards_param.total + reward;
  }

let give_reward (addresses, storage : address list * storage) : operation list * storage =
  let rewards_param : rewards_param = {
    reward = storage.reward;
    token_address = storage.token_address;
    token_id = storage.token_id;
    ops = ([] : operation list);
    total = 0n;
  } in
  let processed_rewards = List.fold process_rewards addresses rewards_param in
  if processed_rewards.total <= storage.funds then (
    processed_rewards.ops, { storage with
      funds = abs(storage.funds - processed_rewards.total);
    }
  ) else (failwith "NO_FUNDS" : operation list * storage)

let add_funds (funds, storage : nat * storage) : operation list * storage =
  let transfer_funds = transfer_fa2(storage.token_address, storage.token_id, funds, Tezos.sender, Tezos.self_address) in
  let ops : operation list = [transfer_funds] in
  ops, { storage with funds = storage.funds + funds; }

let withdraw_funds (funds, storage : nat * storage) : operation list * storage =
  let transfer_funds = transfer_fa2(storage.token_address, storage.token_id, funds, Tezos.self_address, Tezos.sender) in
  let ops : operation list = [transfer_funds] in
  ops, { storage with funds = abs(storage.funds - funds); }

let reward_main (p, storage : reward_entrypoints * storage) : operation list * storage = match p with
  | Reward addresses ->
      let _u : unit = fail_if_paused(storage.admin) in
      let _c : unit = fail_if_not_admin(storage.admin) in
      give_reward(addresses, storage)

  | Add_funds funds ->
      let _u : unit = fail_if_not_admin(storage.admin) in
      add_funds(funds, storage)

  | Withdraw_funds funds ->
      let _u : unit = fail_if_not_admin(storage.admin) in
      withdraw_funds(funds, storage)

  | Update_reward reward ->
      let _u : unit = fail_if_not_admin(storage.admin) in
      ([] : operation list), { storage with reward = reward; }

  | Admin a ->
      let ops, admin = pauseable_admin(a, storage.admin) in
      let new_storage = { storage with admin = admin; } in
      ops, new_storage

let sample_storage : storage = {
  admin = Some ({
    admin = ("tz1MjHeKJwwmAmMHzRuU9JXRjYKH6d3QxLKf" : address);
    pending_admin = (None : address option);
    paused = false;
  });
  funds = 0n;
  reward = 100000n;
  token_address = ("KT1GCibU4MkPYzeuosuQpAymL73Wr8mHnQuv" : address);
  token_id = 0n;
}
