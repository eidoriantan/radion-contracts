
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

type beneficiary =
[@layout:comb]
{
  address: address;
  percentage: nat;
}

type asset =
[@layout:comb]
{
  token_owner: address;
  token_address: address;
  token_id: token_id;
}

type offer =
[@layout:comb]
{
  offer_address: address;
  offer_quantity: nat;
  offer_token_amount: nat;
  offer_amount: tez;
  offer_date: timestamp;
}

type offers = offer list

type asset_param =
[@layout:comb]
{
  token_owner: address;
  token_address: address;
  token_id: token_id;
  token_amount: nat;
  quantity: nat;
}

type offer_param =
[@layout:comb]
{
  token_owner: address;
  token_address: address;
  token_id: token_id;
  offer_address: address;
  offer_amount: tez;
  offer_token_amount: nat;
  offer_quantity: nat;
  offer_date: timestamp;
}

type accept_param =
[@layout:comb]
{
  token_owner: address;
  token_address: address;
  token_id: token_id;
  offer_address: address;
  offer_amount: tez;
  offer_token_amount: nat;
  offer_quantity: nat;
  offer_date: timestamp;
  royalty: nat;
  beneficiaries: beneficiary list;
}

type remove_offer_param =
[@layout:comb]
{
  offer: offer;
  offers: offers;
  operations: operation list;
  token_address: address;
  token_id: token_id;
}

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  offers: (asset, offers) big_map;
  commission: nat;
  max_royalty: nat;
  residual_address: address;
  allowed_token: address;
  allowed_token_id: nat;
}

type offer_entrypoints =
  | Offer of asset_param
  | Accept of accept_param
  | Remove of offer_param
  | Set_commission of nat
  | Set_max_royalty of nat
  | Set_residual of address
  | Admin of pauseable_admin

let offer_exists ((exist, user_offer), offer : (bool * offer) * offer) : bool * offer =
  if user_offer = offer then true, user_offer
  else exist, user_offer

let offer_asset (asset_param, storage : asset_param * storage) : operation list * storage =
  let asset : asset = {
    token_owner = asset_param.token_owner;
    token_address = asset_param.token_address;
    token_id = asset_param.token_id;
  } in
  let offer : offer = {
    offer_address = Tezos.sender;
    offer_amount = Tezos.amount;
    offer_token_amount = asset_param.token_amount;
    offer_quantity = asset_param.quantity;
    offer_date = Tezos.now;
  } in
  let _u : unit = assert_with_error (Tezos.amount <> 0mutez || asset_param.token_amount <> 0n) "AMOUNT_ZERO" in
  let _v : unit = assert_with_error (asset_param.quantity > 0n) "OFFER_QUANTITY" in
  let _c : unit = assert_with_error (asset.token_owner <> Tezos.sender) "OWNER_CANT_OFFER" in
  let oplist = if asset_param.token_amount > 0n then
    let tx_fa2 = transfer_fa2(storage.allowed_token, storage.allowed_token_id, asset_param.token_amount, Tezos.sender, Tezos.self_address) in
    [tx_fa2] else ([] : operation list) in
  let new_offers : (asset, offers) big_map = (match Big_map.find_opt asset storage.offers with
    | None -> Big_map.update asset (Some [offer]) storage.offers
    | Some offers ->
        let exist, _o = List.fold offer_exists offers (false, offer) in
        let _u : unit = assert_with_error (not exist) "OFFER_ALREADY_EXISTS" in
        Big_map.update asset (Some (offer :: offers)) storage.offers
  ) in
  oplist, { storage with offers = new_offers; }

let remove_single_offer (remove_offer_param, offer : remove_offer_param * offer) : remove_offer_param =
  if offer = remove_offer_param.offer then
    let ops = if offer.offer_amount > 0mutez then transfer_tez(offer.offer_amount, offer.offer_address) :: remove_offer_param.operations else remove_offer_param.operations in
    let oplist = if offer.offer_token_amount > 0n then transfer_fa2(remove_offer_param.token_address, remove_offer_param.token_id, offer.offer_token_amount, Tezos.self_address, offer.offer_address) :: ops else ops in
    { remove_offer_param with operations = oplist; }
  else { remove_offer_param with offers = offer :: remove_offer_param.offers; }

let remove_offers_except (remove_offer_param, offer : remove_offer_param * offer) : remove_offer_param =
  if offer <> remove_offer_param.offer then
    let ops = if offer.offer_amount > 0mutez then transfer_tez(offer.offer_amount, offer.offer_address) :: remove_offer_param.operations else remove_offer_param.operations in
    let oplist = if offer.offer_token_amount > 0n then transfer_fa2(remove_offer_param.token_address, remove_offer_param.token_id, offer.offer_token_amount, Tezos.self_address, offer.offer_address) :: ops else ops in
    { remove_offer_param with operations = oplist; }
  else
    { remove_offer_param with offers = offer :: remove_offer_param.offers; }

type royalty_fold_tez =
[@layout:comb]
{
  ops: operation list;
  amount: tez;
  total: tez;
  total_percent: nat;
}

type royalty_fold_token =
[@layout:comb]
{
  ops: operation list;
  amount: nat;
  total: nat;
  total_percent: nat;
  token_address: address;
  token_id: token_id;
}

let process_royalties_tez (royalty, beneficiary : royalty_fold_tez * beneficiary) : royalty_fold_tez =
  let royalty_price : tez = royalty.amount * beneficiary.percentage / 10000n in
  let oplist = if royalty_price > 0mutez then transfer_tez(royalty_price, beneficiary.address) :: royalty.ops else royalty.ops in
  { royalty with
    ops = oplist;
    total = royalty.total + royalty_price;
    total_percent = royalty.total_percent + beneficiary.percentage;
  }

let process_royalties_token (royalty, beneficiary : royalty_fold_token * beneficiary) : royalty_fold_token =
  let royalty_price : nat = royalty.amount * beneficiary.percentage / 10000n in
  let oplist = if royalty_price > 0n then transfer_fa2(royalty.token_address, royalty.token_id, royalty_price, Tezos.self_address, beneficiary.address) :: royalty.ops else royalty.ops in
  { royalty with
    ops = oplist;
    total = royalty.total + royalty_price;
    total_percent = royalty.total_percent + beneficiary.percentage;
  }

let merge_lists (list1, element : operation list * operation) : operation list =
  element :: list1

let complete_offer (asset, offer, offers, royalty, beneficiaries, storage : asset * offer * offers * nat * beneficiary list * storage) : operation list * storage =
  let transfer_nft = transfer_fa2(asset.token_address, asset.token_id, offer.offer_quantity, asset.token_owner, offer.offer_address) in
  match storage.admin with
    | None ->
      let royalty_total_tez = if royalty <> 0n then offer.offer_amount * royalty / 10000n else offer.offer_amount in
      let royalty_total_token = if royalty <> 0n then offer.offer_token_amount * royalty / 10000n else offer.offer_token_amount in
      let seller_amount_tez : tez = offer.offer_amount - royalty_total_tez in
      let seller_amount_token : nat = abs(offer.offer_token_amount - royalty_total_token) in
      let royalty_fold_tez : royalty_fold_tez = {
        ops = ([] : operation list);
        amount = royalty_total_tez;
        total = 0tez;
        total_percent = 0n;
      } in
      let royalty_fold_token : royalty_fold_token = {
        ops = ([] : operation list);
        amount = royalty_total_token;
        total = 0n;
        total_percent = 0n;
        token_address = storage.allowed_token;
        token_id = storage.allowed_token_id;
      } in
      let processed_royalties_tez = List.fold process_royalties_tez beneficiaries royalty_fold_tez in
      let processed_royalties_token = List.fold process_royalties_token beneficiaries royalty_fold_token in
      let royalty_rate_tez : nat = if offer.offer_amount > 0mutez then processed_royalties_tez.total / offer.offer_amount * 10000n else 0n in
      let royalty_rate_token : nat = if offer.offer_token_amount > 0n then processed_royalties_token.total / offer.offer_token_amount * 10000n else 0n in
      let residual_tez : tez = offer.offer_amount - processed_royalties_tez.total - seller_amount_tez in
      let residual_token : nat = abs(offer.offer_token_amount - processed_royalties_token.total - seller_amount_token) in
      let _ctz : unit = if royalty <> 0n && royalty_rate_tez > storage.max_royalty then (failwith "INVALID ROYALTY" : unit) else () in
      let _utz : unit = if processed_royalties_tez.total_percent <> 10000n then (failwith "INVALID BENEFICIARIES" : unit) else () in
      let _ctx : unit = if royalty <> 0n && royalty_rate_token > storage.max_royalty then (failwith "INVALID ROYALTY" : unit) else () in
      let _utx : unit = if processed_royalties_token.total_percent <> 10000n then (failwith "INVALID BENEFICIARIES" : unit) else () in
      let ops = List.fold merge_lists processed_royalties_token.ops processed_royalties_tez.ops in
      let ops_1 = if seller_amount_tez > 0mutez then transfer_tez(seller_amount_tez, asset.token_owner) :: ops else ops in
      let ops_2 = if seller_amount_token > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, seller_amount_token, Tezos.self_address, asset.token_owner) :: ops_1 else ops_1 in
      let ops_3 = if residual_tez > 0mutez then transfer_tez(residual_tez, storage.residual_address) :: ops_2 else ops_2 in
      let ops_4 = if residual_token > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, residual_token, Tezos.self_address, storage.residual_address) :: ops_3 else ops_3 in
      let remove_offer_param : remove_offer_param = {
        offer = offer;
        offers = ([] : offers);
        operations = ops_4;
        token_address = storage.allowed_token;
        token_id = storage.allowed_token_id;
      } in
      let returns = List.fold remove_offers_except offers remove_offer_param in
      let new_storage = { storage with offers = Big_map.remove asset storage.offers; } in
      transfer_nft :: returns.operations, new_storage
    | Some admin ->
      let commission_tez : tez = offer.offer_amount * storage.commission / 10000n in
      let commission_token : nat = offer.offer_token_amount * storage.commission / 10000n in
      let left_tez : tez = offer.offer_amount - commission_tez in
      let left_token : nat = abs(offer.offer_token_amount - commission_token) in
      let royalty_total_tez = if royalty <> 0n then offer.offer_amount * royalty / 10000n else left_tez in
      let royalty_total_token = if royalty <> 0n then offer.offer_token_amount * royalty / 10000n else left_token in
      let seller_amount_tez : tez = offer.offer_amount - royalty_total_tez - commission_tez in
      let seller_amount_token : nat = abs(offer.offer_token_amount - royalty_total_token - commission_token) in
      let royalty_fold_tez : royalty_fold_tez = {
        ops = ([] : operation list);
        amount = royalty_total_tez;
        total = 0tez;
        total_percent = 0n;
      } in
      let royalty_fold_token : royalty_fold_token = {
        ops = ([] : operation list);
        amount = royalty_total_token;
        total = 0n;
        total_percent = 0n;
        token_address = storage.allowed_token;
        token_id = storage.allowed_token_id;
      } in
      let processed_royalties_tez = List.fold process_royalties_tez beneficiaries royalty_fold_tez in
      let processed_royalties_token = List.fold process_royalties_token beneficiaries royalty_fold_token in
      let royalty_rate_tez : nat = if offer.offer_amount > 0mutez then processed_royalties_tez.total / offer.offer_amount * 10000n else 0n in
      let royalty_rate_token : nat = if offer.offer_token_amount > 0n then processed_royalties_token.total / offer.offer_token_amount * 10000n else 0n in
      let residual_tez : tez = offer.offer_amount - commission_tez - processed_royalties_tez.total - seller_amount_tez in
      let residual_token : nat = abs(offer.offer_token_amount - commission_token - processed_royalties_token.total - seller_amount_token) in
      let _ctz : unit = if royalty <> 0n && royalty_rate_tez > storage.max_royalty then (failwith "INVALID ROYALTY" : unit) else () in
      let _utz : unit = if processed_royalties_tez.total_percent <> 10000n then (failwith "INVALID BENEFICIARIES" : unit) else () in
      let _ctx : unit = if royalty <> 0n && royalty_rate_token > storage.max_royalty then (failwith "INVALID ROYALTY" : unit) else () in
      let _utx : unit = if processed_royalties_token.total_percent <> 10000n then (failwith "INVALID BENEFICIARIES" : unit) else () in
      let ops = List.fold merge_lists processed_royalties_token.ops processed_royalties_tez.ops in
      let ops_1 = if seller_amount_tez > 0mutez then transfer_tez(seller_amount_tez, asset.token_owner) :: ops else ops in
      let ops_2 = if seller_amount_token > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, seller_amount_token, Tezos.self_address, asset.token_owner) :: ops_1 else ops_1 in
      let ops_3 = if commission_tez > 0mutez then transfer_tez(commission_tez, admin.admin) :: ops_2 else ops_2 in
      let ops_4 = if commission_token > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, commission_token, Tezos.self_address, admin.admin) :: ops_3 else ops_3 in
      let ops_5 = if residual_tez > 0mutez then transfer_tez(residual_tez, storage.residual_address) :: ops_4 else ops_4 in
      let ops_6 = if residual_token > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, residual_token, Tezos.self_address, storage.residual_address) :: ops_5 else ops_5 in
      let remove_offer_param : remove_offer_param = {
        offer = offer;
        offers = ([] : offers);
        operations = ops_6;
        token_address = storage.allowed_token;
        token_id = storage.allowed_token_id;
      } in
      let returns = List.fold remove_offers_except offers remove_offer_param in
      let new_storage = { storage with offers = Big_map.remove asset storage.offers; } in
      transfer_nft :: returns.operations, new_storage

let accept_offer (offer_param, storage : accept_param * storage) : operation list * storage =
  let asset : asset = {
    token_owner = offer_param.token_owner;
    token_address = offer_param.token_address;
    token_id = offer_param.token_id;
  } in
  let offer : offer = {
    offer_address = offer_param.offer_address;
    offer_amount = offer_param.offer_amount;
    offer_token_amount = offer_param.offer_token_amount;
    offer_quantity = offer_param.offer_quantity;
    offer_date = offer_param.offer_date;
  } in
  match Big_map.find_opt asset storage.offers with
    | None -> (failwith "NO_OFFER" : operation list * storage)
    | Some offers ->
        let _u : unit = assert_with_error (asset.token_owner = Tezos.sender) "NOT_OWNER" in
        let exist, _o = List.fold offer_exists offers (false, offer) in
        if exist then complete_offer(asset, offer, offers, offer_param.royalty, offer_param.beneficiaries, storage)
        else (failwith "NO_OFFER" : operation list * storage)

let remove_offer (offer_param, storage : offer_param * storage) : operation list * storage =
  let asset : asset = {
    token_owner = offer_param.token_owner;
    token_address = offer_param.token_address;
    token_id = offer_param.token_id;
  } in
  let offer : offer = {
    offer_address = offer_param.offer_address;
    offer_amount = offer_param.offer_amount;
    offer_token_amount = offer_param.offer_token_amount;
    offer_quantity = offer_param.offer_quantity;
    offer_date = offer_param.offer_date;
  } in
  match Big_map.find_opt asset storage.offers with
    | None -> (failwith "NO_OFFER" : operation list * storage)
    | Some offers ->
        let _u : unit = assert_with_error (offer.offer_address = Tezos.sender) "NOT_OWNER" in
        let exist, _o = List.fold offer_exists offers (false, offer) in
        if exist then
          let remove_offer_param : remove_offer_param = {
            offer = offer;
            offers = ([] : offers);
            operations = ([] : operation list);
            token_address = storage.allowed_token;
            token_id = storage.allowed_token_id;
          } in
          let removed_offer = List.fold remove_single_offer offers remove_offer_param in
          let new_offers = Big_map.update asset (Some removed_offer.offers) storage.offers in
          let new_storage = { storage with offers = new_offers; } in
          removed_offer.operations, new_storage
        else (failwith "NO_OFFER" : operation list * storage)

let set_commission (commission, storage : nat * storage) : operation list * storage =
  let new_s = { storage with commission = commission; } in
  ([] : operation list), new_s

let set_royalty (royalty, storage : nat * storage) : operation list * storage =
  let new_s = { storage with max_royalty = royalty; } in
  ([] : operation list), new_s

let set_residual (addr, storage : address * storage) : operation list * storage =
  let new_s = { storage with residual_address = addr; } in
  ([] : operation list), new_s

let offer_main (p, storage : offer_entrypoints * storage) : operation list * storage = match p with
  | Offer asset ->
      let _u : unit = fail_if_paused(storage.admin) in
      offer_asset(asset, storage)
  | Accept offer ->
      let _u : unit = fail_if_paused(storage.admin) in
      accept_offer(offer, storage)
  | Remove offer ->
      let _u : unit = fail_if_paused(storage.admin) in
      remove_offer(offer, storage)
  | Set_commission commission ->
      let _u : unit = fail_if_not_admin(storage.admin) in
      set_commission(commission, storage)
  | Set_max_royalty royalty ->
      let _u : unit = fail_if_not_admin(storage.admin) in
      set_royalty(royalty, storage)
  | Set_residual addr ->
      let _u : unit = fail_if_not_admin(storage.admin) in
      set_residual(addr, storage)
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
  offers = (Big_map.empty : (asset, offers) big_map);
  commission = 1000n;
  max_royalty = 2000n;
  residual_address = ("tz1TkJxyXXj5DtrmKvvNc1eqcEPV5uMs8xkM" : address);
  allowed_token = ("KT1GCibU4MkPYzeuosuQpAymL73Wr8mHnQuv": address);
  allowed_token_id = 0n;
}
