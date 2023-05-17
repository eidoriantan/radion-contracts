
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
            if (Tezos.get_sender() : address) = pending
            then (Some ({s with
              pending_admin = (None : address option);
              admin = (Tezos.get_sender() : address);
            } : pauseable_admin_storage_record))
            else (failwith "NOT_A_PENDING_ADMIN" : pauseable_admin_storage))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let fail_if_not_admin (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if (Tezos.get_sender() : address) <> a.admin
        then failwith "NOT_AN_ADMIN"
        else unit
    | None -> unit

let fail_if_not_admin_ext (storage, extra_msg : pauseable_admin_storage * string) : unit =
  match storage with
    | Some a ->
        if (Tezos.get_sender() : address) <> a.admin
        then failwith ("NOT_AN_ADMIN" ^  " "  ^ extra_msg)
        else unit
    | None -> unit

let set_admin (new_admin, storage : address * pauseable_admin_storage) : pauseable_admin_storage =
  let _u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with pending_admin = Some new_admin; } : pauseable_admin_storage_record))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let pause (paused, storage: bool * pauseable_admin_storage) : pauseable_admin_storage =
  let _u = fail_if_not_admin storage in
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

  | Confirm_admin _u ->
      let new_s = confirm_new_admin storage in
      (([]: operation list), new_s)

  | Pause paused ->
      let new_s = pause (paused, storage) in
      (([]: operation list), new_s)

type royalty =
[@layout:comb]
{
  address: address;
  percentage: nat;
}

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  residual_address: address;
}

type distribute_param =
[@layout:comb]
{
  royalties: royalty list;
  amount: tez;
}

type royalties_entrypoints =
  | Distribute of distribute_param
  | Set_residual of address
  | Admin of pauseable_admin

type royalty_fold =
[@layout:comb]
{
  ops: operation list;
  amount: tez;
  total: tez;
  total_percent: nat;
}

let process_royalties (royalty_fold, royalty : royalty_fold * royalty) : royalty_fold =
  let royalty_price : tez = royalty_fold.amount * royalty.percentage / 10000n in
  let oplist = if royalty_price > 0mutez then transfer_tez(royalty_price, royalty.address) :: royalty_fold.ops else royalty_fold.ops in
  { royalty_fold with
    ops = oplist;
    total = royalty_fold.total + royalty_price;
    total_percent = royalty_fold.total_percent + royalty.percentage;
  }

let distribute (royalties, storage : distribute_param * storage) : operation list * storage =
  let _u : unit = assert_with_error ((Tezos.get_amount() : tez) <> royalties.amount) "AMOUNT_UNMATCH" in
  let royalty_fold : royalty_fold = {
    ops = ([] : operation list);
    amount = royalties.amount;
    total = 0tez;
    total_percent = 0n;
  } in
  let processed_royalties = List.fold process_royalties royalties.royalties royalty_fold in
  let residual : tez = match (royalties.amount - processed_royalties.total) with
    | None -> 0tez
    | Some left -> left in
  let oplist : operation list = if residual > 0tez then transfer_tez(residual, storage.residual_address) :: processed_royalties.ops else processed_royalties.ops in
  oplist, storage

let set_residual (residual, storage : address * storage) : operation list * storage =
  let new_storage = { storage with residual_address = residual; } in
  ([] : operation list), new_storage

let royalties_main (p, storage : royalties_entrypoints * storage) : operation list * storage = match p with
  | Distribute royalties ->
      let _u : unit = fail_if_paused(storage.admin) in
      distribute(royalties, storage)
  | Set_residual residual ->
      let _u : unit = fail_if_not_admin(storage.admin) in
      set_residual(residual, storage)
  | Admin a ->
      let ops, admin = pauseable_admin(a, storage.admin) in
      let new_storage = { storage with admin = admin; } in
      ops, new_storage

let sample_storage : storage = {
  admin = Some ({
    admin = ("tz1TkJxyXXj5DtrmKvvNc1eqcEPV5uMs8xkM" : address);
    pending_admin = (None : address option);
    paused = false;
  });
  residual_address = ("tz1TkJxyXXj5DtrmKvvNc1eqcEPV5uMs8xkM" : address);
}
