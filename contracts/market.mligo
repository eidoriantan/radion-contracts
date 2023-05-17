
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

(* `pauseable_admin` entry points *)
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
  let sender = Tezos.get_sender() in
  match storage with
    | Some s ->
        ( match s.pending_admin with
          | None -> (failwith "NO_PENDING_ADMIN" : pauseable_admin_storage)
          | Some pending ->
            if sender = pending
            then (Some ({s with
              pending_admin = (None : address option);
              admin = sender;
            } : pauseable_admin_storage_record))
            else (failwith "NOT_A_PENDING_ADMIN" : pauseable_admin_storage))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let fail_if_not_admin (storage : pauseable_admin_storage) : unit =
  let sender = Tezos.get_sender() in
  match storage with
    | Some a ->
        if sender <> a.admin
        then failwith "NOT_AN_ADMIN"
        else unit
    | None -> unit

let fail_if_not_admin_ext (storage, extra_msg : pauseable_admin_storage * string) : unit =
  let sender = Tezos.get_sender() in
  match storage with
    | Some a ->
        if sender <> a.admin
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

type fee_data =
  [@layout:comb]
  {
    fee_address : address;
    fee_percent : nat;
  }

type fa2_tokens =
  [@layout:comb]
  {
    token_id : token_id;
    amount : nat;
  }

type tokens =
  [@layout:comb]
  {
    fa2_address : address;
    fa2_batch : (fa2_tokens list);
  }

type global_token_id =
  [@layout:comb]
  {
      fa2_address : address;
      token_id : token_id;
  }

let ceil_div_nat (numerator, denominator : nat * nat) : nat = abs ((- numerator) / (int denominator))

let percent_of_bid_nat (percent, bid : nat * nat) : nat =
  (ceil_div_nat (bid *  percent, 100n))

let ceil_div_tez (tz_qty, nat_qty : tez * nat) : tez =
  let ediv1 : (tez * tez) option = ediv tz_qty nat_qty in
  match ediv1 with
    | None -> (failwith "DIVISION_BY_ZERO"  : tez)
    | Some e ->
       let (quotient, remainder) = e in
       if remainder > 0mutez then (quotient + 1mutez) else quotient

let percent_of_bid_tez (percent, bid : nat * tez) : tez =
  (ceil_div_tez (bid *  percent, 100n))

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let address_to_contract_transfer_entrypoint(add : address) : ((transfer list) contract) =
  let c : (transfer list) contract option = Tezos.get_entrypoint_opt "%transfer" add in
  match c with
    None -> (failwith "Invalid FA2 Address" : (transfer list) contract)
  | Some c ->  c

let resolve_contract (add : address) : unit contract =
  match ((Tezos.get_contract_opt add) : (unit contract) option) with
      None -> (failwith "Return address does not resolve to contract" : unit contract)
    | Some c -> c

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

type sale_token_param_tez =
[@layout:comb]
{
 token_for_sale_address: address;
 token_for_sale_token_id: token_id;
}

type sale_param_tez =
[@layout:comb]
{
  seller: address;
  sale_token: sale_token_param_tez;
}

type sale_param_token =
[@layout:comb]
{
  seller: address;
  sale_token: sale_token_param_tez;
  token_amount: nat;
}

type cancel_param =
[@layout:comb]
{
  seller: address;
  sale_token: sale_token_param_tez;
  token_quantity: nat;
}

type royalty_param =
[@layout:comb]
{
  owner: address;
  rate: nat;
}

type royalty_tez =
[@layout:comb]
{
  address: address;
  percentage: nat;
}

type royalties_tez =
[@layout:comb]
{
  amount: tez;
  quantity: nat;
  second: nat;
  royalties: royalty_tez list;
  commissioners: royalty_tez list;
}

type distributes =
[@layout:comb]
{
  royalties: royalty_tez list;
  commissioners: royalty_tez list;
}

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  commission_tenthousandth: nat;
  max_royalty: nat;
  sales: (sale_param_tez, royalties_tez) big_map;
  residual_address: address;
  allowed_token: address;
  allowed_token_id: nat;
}

type init_sale_param_tez =
[@layout:comb]
{
  sale_price: tez;
  sale_token_param_tez: sale_token_param_tez;
  second: nat;
  quantity: nat;
  royalties: royalty_tez list;
  commissioners: royalty_tez list;
}

type init_sales_param_tez = init_sale_param_tez list
type ops = operation list

type market_entry_points =
  | Sell of init_sales_param_tez
  | Buy of sale_param_tez
  | Buy_token of sale_param_token
  | Set_commission of nat
  | Set_max_royalty of nat
  | Cancel of cancel_param
  | Set_residual of address
  | Admin of pauseable_admin

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let transfer_nft(fa2_address, token_id, from, to_: address * token_id * address * address): operation =
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
        amount = 1n;
    }]} in
    Tezos.transaction [tx] 0mutez c in
  transfer_op

let transfer_tez (qty, to_ : tez * address) : operation =
  let destination = (match (Tezos.get_contract_opt to_ : unit contract option) with
    | None -> (failwith "ADDRESS_DOES_NOT_RESOLVE" : unit contract)
    | Some acc -> acc) in
  Tezos.transaction () qty destination

type royalty_fold =
[@layout:comb]
{
  ops: operation list;
  sale_price: tez;
  total: tez;
}

type royalty_fold_token =
[@layout:comb]
{
  ops: operation list;
  sale_price: nat;
  total: nat;
  token_address: address;
  token_id: nat;
}

let process_royalties (royalty, royalty_tez : royalty_fold * royalty_tez) : royalty_fold =
  let royalty_price : tez = royalty.sale_price * royalty_tez.percentage / 10000n in
  let tx_royalty = transfer_tez(royalty_price, royalty_tez.address) in
  { royalty with
    ops = tx_royalty :: royalty.ops;
    total = royalty.total + royalty_price;
  }

let process_royalties_token (royalty, royalty_tez : royalty_fold_token * royalty_tez) : royalty_fold_token =
  let sender = Tezos.get_sender() in
  let royalty_price : nat = royalty.sale_price * royalty_tez.percentage / 10000n in
  let tx_royalty = transfer_fa2(royalty.token_address, royalty.token_id, royalty_price, sender, royalty_tez.address) in
  { royalty with
    ops = tx_royalty :: royalty.ops;
    total = royalty.total + royalty_price;
  }

let buy_token(sale, storage: sale_param_tez * storage) : (operation list * storage) =
  let sender = Tezos.get_sender() in
  let self_address = Tezos.get_self_address() in
  let tezAmount = Tezos.get_amount() in
  let royalties_tez : royalties_tez = (match Big_map.find_opt sale storage.sales with
  | None -> (failwith "NO_SALE" : royalties_tez)
  | Some s -> s) in
  let sale_price = royalties_tez.amount in
  let _amountError : unit =
    if tezAmount <> sale_price
    then ([%Michelson ({| { FAILWITH } |} : string * tez * tez -> unit)] ("WRONG_TEZ_PRICE", sale_price, tezAmount) : unit)
    else () in
  let tx_nft = transfer_nft(sale.sale_token.token_for_sale_address, sale.sale_token.token_for_sale_token_id, self_address, sender) in
  match storage.admin with
  | None ->
      let royalty_total_price : tez = if royalties_tez.second <> 0n then sale_price * royalties_tez.second / 10000n else sale_price in
      let seller_amount : tez = Option.unopt(sale_price - royalty_total_price) in
      let royalty_fold : royalty_fold = {
        ops = [tx_nft];
        sale_price = royalty_total_price;
        total = 0tez;
      } in
      let processed_royalties = List.fold process_royalties royalties_tez.royalties royalty_fold in
      let royalty_total_rate : nat = processed_royalties.total / tezAmount * 10000n in
      let residual_a = Option.unopt(tezAmount - processed_royalties.total) in
      let residual = Option.unopt(residual_a - seller_amount) in
      let _c : unit = if royalties_tez.second <> 0n && royalty_total_rate > storage.max_royalty then (failwith "INVALID_ROYALTY" : unit) else () in
      let oplist = if seller_amount > 0tez then transfer_tez(seller_amount, sale.seller) :: processed_royalties.ops else processed_royalties.ops in
      let oplist_2 = if residual > 0tez then transfer_tez(residual, storage.residual_address) :: oplist else oplist in
      let new_storage = if royalties_tez.quantity = 1n then { storage with sales = Big_map.remove sale storage.sales }
        else (
          let new_royalties_tez = { royalties_tez with quantity = abs(royalties_tez.quantity - 1n); } in
          { storage with sales = Big_map.update sale (Some new_royalties_tez) storage.sales }
        ) in
      oplist_2, new_storage
  | Some admin ->
      let commission_price : tez = sale_price * storage.commission_tenthousandth / 10000n in
      let left_price : tez = Option.unopt(sale_price - commission_price) in
      let royalty_total_price : tez = if royalties_tez.second <> 0n then sale_price * royalties_tez.second / 10000n else left_price in
      let seller_amount_a = Option.unopt(sale_price - royalty_total_price) in
      let seller_amount = Option.unopt(seller_amount_a - commission_price) in
      let commission_fold : royalty_fold = {
        ops = [tx_nft];
        sale_price = commission_price;
        total = 0tez;
      } in
      let processed_commissioners = List.fold process_royalties royalties_tez.commissioners commission_fold in
      let royalty_fold : royalty_fold = {
        ops = processed_commissioners.ops;
        sale_price = royalty_total_price;
        total = 0tez;
      } in
      let processed_royalties = List.fold process_royalties royalties_tez.royalties royalty_fold in
      let admin_commission : tez = Option.unopt(commission_price - processed_commissioners.total) in
      let royalty_total_rate : nat = processed_royalties.total / tezAmount * 10000n in
      let residual_a = Option.unopt(tezAmount - commission_price) in
      let residual_b = Option.unopt(residual_a - processed_royalties.total) in
      let residual = Option.unopt(residual_b - seller_amount) in
      let _c : unit = if royalties_tez.second <> 0n && royalty_total_rate > storage.max_royalty then (failwith "INVALID_ROYALTY" : unit) else () in
      let ops = if seller_amount > 0tez then transfer_tez(seller_amount, sale.seller) :: processed_royalties.ops else processed_royalties.ops in
      let oplist = if admin_commission > 0tez then transfer_tez(admin_commission, admin.admin) :: ops else ops in
      let oplist_2 = if residual > 0tez then transfer_tez(residual, storage.residual_address) :: oplist else oplist in
      let new_storage = if royalties_tez.quantity = 1n then { storage with sales = Big_map.remove sale storage.sales }
        else (
          let new_royalties_tez = { royalties_tez with quantity = abs(royalties_tez.quantity - 1n); } in
          { storage with sales = Big_map.update sale (Some new_royalties_tez) storage.sales }
        ) in
      oplist_2, new_storage

let buy_token_with_token(sale, storage: sale_param_token * storage) : (operation list * storage) =
  let sender = Tezos.get_sender() in
  let self_address = Tezos.get_self_address() in
  let sale_find : sale_param_tez = {
    seller = sale.seller;
    sale_token = {
      token_for_sale_address = sale.sale_token.token_for_sale_address;
      token_for_sale_token_id = sale.sale_token.token_for_sale_token_id;
    }
  } in
  let royalties_tez : royalties_tez = (match Big_map.find_opt sale_find storage.sales with
  | None -> (failwith "NO_SALE" : royalties_tez)
  | Some s -> s) in
  let sale_price = sale.token_amount in
  let tx_nft = transfer_nft(sale.sale_token.token_for_sale_address, sale.sale_token.token_for_sale_token_id, self_address, sender) in
  match storage.admin with
  | None ->
      let royalty_total_price : nat = if royalties_tez.second <> 0n then sale_price * royalties_tez.second / 10000n else sale_price in
      let seller_amount : nat = abs(sale_price - royalty_total_price) in
      let royalty_fold : royalty_fold_token = {
        ops = [tx_nft];
        sale_price = royalty_total_price;
        total = 0n;
        token_address = storage.allowed_token;
        token_id = storage.allowed_token_id;
      } in
      let processed_royalties = List.fold process_royalties_token royalties_tez.royalties royalty_fold in
      let royalty_total_rate : nat = processed_royalties.total / sale_price * 10000n in
      let residual : nat = abs(sale_price - processed_royalties.total - seller_amount) in
      let _c : unit = if royalties_tez.second <> 0n && royalty_total_rate > storage.max_royalty then (failwith "INVALID_ROYALTY" : unit) else () in
      let oplist = if seller_amount > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, seller_amount, sender, sale.seller) :: processed_royalties.ops else processed_royalties.ops in
      let oplist_2 = if residual > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, residual, sender, storage.residual_address) :: oplist else oplist in
      let new_storage = if royalties_tez.quantity = 1n then { storage with sales = Big_map.remove sale_find storage.sales }
        else (
          let new_royalties_tez = { royalties_tez with quantity = abs(royalties_tez.quantity - 1n); } in
          { storage with sales = Big_map.update sale_find (Some new_royalties_tez) storage.sales }
        ) in
      oplist_2, new_storage
  | Some admin ->
      let commission_price : nat = sale_price * storage.commission_tenthousandth / 10000n in
      let left_price : nat = abs(sale_price - commission_price) in
      let royalty_total_price : nat = if royalties_tez.second <> 0n then sale_price * royalties_tez.second / 10000n else left_price in
      let seller_amount : nat = abs(sale_price - royalty_total_price - commission_price) in
      let commission_fold : royalty_fold_token = {
        ops = [tx_nft];
        sale_price = commission_price;
        total = 0n;
        token_address = storage.allowed_token;
        token_id = storage.allowed_token_id;
      } in
      let processed_commissioners = List.fold process_royalties_token royalties_tez.commissioners commission_fold in
      let royalty_fold : royalty_fold_token = {
        ops = processed_commissioners.ops;
        sale_price = royalty_total_price;
        total = 0n;
        token_address = storage.allowed_token;
        token_id = storage.allowed_token_id;
      } in
      let processed_royalties = List.fold process_royalties_token royalties_tez.royalties royalty_fold in
      let admin_commission = abs(commission_price - processed_commissioners.total) in
      let royalty_total_rate : nat = processed_royalties.total / sale_price * 10000n in
      let residual : nat = abs(sale_price - commission_price - processed_royalties.total - seller_amount) in
      let _c : unit = if royalties_tez.second <> 0n && royalty_total_rate > storage.max_royalty then (failwith "INVALID_ROYALTY" : unit) else () in
      let ops = if seller_amount > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, seller_amount, sender, sale.seller) :: processed_royalties.ops else processed_royalties.ops in
      let oplist = if admin_commission > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, admin_commission, sender, admin.admin) :: ops else ops in
      let oplist_2 = if residual > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, residual, sender, storage.residual_address) :: oplist else oplist in
      let new_storage = if royalties_tez.quantity = 1n then { storage with sales = Big_map.remove sale_find storage.sales }
        else (
          let new_royalties_tez = { royalties_tez with quantity = abs(royalties_tez.quantity - 1n); } in
          { storage with sales = Big_map.update sale_find (Some new_royalties_tez) storage.sales }
        ) in
      oplist_2, new_storage

let tez_stuck_guard(entrypoint: string) : string = "DON'T TRANSFER TEZ TO THIS ENTRYPOINT (" ^ entrypoint ^ ")"

let deposit_for_sale(ops, sale : ops * init_sale_param_tez) : operation list =
    let sender = Tezos.get_sender() in
    let self_address = Tezos.get_self_address() in
    let sale_token : sale_token_param_tez = sale.sale_token_param_tez in
    let transfer_op =
      transfer_fa2 (sale_token.token_for_sale_address, sale_token.token_for_sale_token_id, sale.quantity, sender, self_address) in
    transfer_op :: ops

let royalty_percentage (total_percentage, royalty : nat * royalty_tez) : nat =
    total_percentage + royalty.percentage

let deposit_for_sale_storage(storage, sale : storage * init_sale_param_tez) : storage =
    let sender = Tezos.get_sender() in
    let sale_token : sale_token_param_tez = sale.sale_token_param_tez in
    let royalties_tez : royalties_tez = {
      amount = sale.sale_price;
      quantity = sale.quantity;
      second = sale.second;
      royalties = sale.royalties;
      commissioners = sale.commissioners;
    } in
    let total_percentage = List.fold royalty_percentage sale.royalties 0n in
    let _u : unit = if total_percentage <> 10000n then (failwith "ROYALTIES_PERCENT" : unit) else () in
    let sale_param = { seller = sender; sale_token = sale_token; } in
    match Big_map.find_opt sale_param storage.sales with
    | Some _royalties_tez -> (failwith "ALREADY_ON_SALE" : storage)
    | None -> { storage with sales = Big_map.add sale_param royalties_tez storage.sales }

let deposit_for_sale_batch(sales, storage : init_sales_param_tez * storage) : (operation list * storage) =
    let tezAmount = Tezos.get_amount() in
    let _u : unit = if tezAmount <> 0tez then failwith (tez_stuck_guard "SELL") else () in
    let ops : ops = List.fold deposit_for_sale sales ([] : ops) in
    let new_s : storage = List.fold deposit_for_sale_storage sales storage in
    ops, new_s

let set_commission(commission, storage : nat * storage) : (operation list * storage) =
    let new_s : storage = { storage with commission_tenthousandth = commission } in
    ([] : operation list), new_s

let set_max_royalty(royalty, storage : nat * storage) : (operation list * storage) =
    let new_s : storage = { storage with max_royalty = royalty } in
    ([] : operation list), new_s

let cancel_sale(cancel_param, storage: cancel_param * storage) : (operation list * storage) =
  let sender = Tezos.get_sender() in
  let self_address = Tezos.get_self_address() in
  let tezAmount = Tezos.get_amount() in
  let _u : unit = if tezAmount <> 0tez then failwith (tez_stuck_guard "CANCEL") else () in
  let sale : sale_param_tez = {
    seller = cancel_param.seller;
    sale_token = {
      token_for_sale_address = cancel_param.sale_token.token_for_sale_address;
      token_for_sale_token_id = cancel_param.sale_token.token_for_sale_token_id;
    }
  } in
  match Big_map.find_opt sale storage.sales with
    | None -> (failwith "NO_SALE" : (operation list * storage))
    | Some royalties_tez -> if sale.seller = sender then
                      let _u : unit = if cancel_param.token_quantity > royalties_tez.quantity then (failwith "INVALID_QUANTITY" : unit) else () in
                      let tx_nft_back_op = transfer_fa2(sale.sale_token.token_for_sale_address, sale.sale_token.token_for_sale_token_id, cancel_param.token_quantity, self_address, sender) in
                      if cancel_param.token_quantity = royalties_tez.quantity then
                        [tx_nft_back_op], { storage with sales = Big_map.remove sale storage.sales }
                      else
                        let new_value = { royalties_tez with quantity = abs(royalties_tez.quantity - cancel_param.token_quantity) } in
                        [tx_nft_back_op], { storage with sales = Big_map.update sale (Some new_value) storage.sales }
      else (failwith "NOT_OWNER": (operation list * storage))

let set_residual (residual_address, storage : address * storage) : operation list * storage =
  let new_s = { storage with residual_address = residual_address; } in
  ([] : operation list), new_s

let fixed_price_sale_tez_main (p, storage : market_entry_points * storage) : operation list * storage = match p with
  | Sell sales ->
    let _u : unit = fail_if_paused(storage.admin) in
    deposit_for_sale_batch(sales, storage)
  | Buy sale ->
    let _u : unit = fail_if_paused(storage.admin) in
    buy_token(sale, storage)
  | Buy_token sale ->
    let _u : unit = fail_if_paused(storage.admin) in
    buy_token_with_token(sale, storage)
  | Set_commission commission ->
    let _u : unit = fail_if_not_admin(storage.admin) in
    set_commission(commission, storage)
  | Set_max_royalty royalty ->
    let _u : unit = fail_if_not_admin(storage.admin) in
    set_max_royalty(royalty, storage)
  | Cancel sale ->
    let sender = Tezos.get_sender() in
    let _u : unit = fail_if_paused(storage.admin) in
    let is_seller = sender = sale.seller in
    let _v : unit = if is_seller then ()
             else fail_if_not_admin_ext (storage.admin, "OR A SELLER") in
    cancel_sale(sale,storage)
  | Set_residual addr ->
    let _u : unit = fail_if_not_admin(storage.admin) in
    set_residual(addr, storage)
  | Admin a ->
     let ops, admin = pauseable_admin(a, storage.admin) in
     let new_storage = { storage with admin = admin; } in
     ops, new_storage

let sample_storage : storage =
  {
    admin = Some ({
        admin =  ("tz1TkJxyXXj5DtrmKvvNc1eqcEPV5uMs8xkM" :address);
        pending_admin = (None : address option);
        paused = false;});
    commission_tenthousandth = 1000n;
    max_royalty = 2000n;
    sales = (Big_map.empty : (sale_param_tez, royalties_tez) big_map);
    residual_address = ("tz1TkJxyXXj5DtrmKvvNc1eqcEPV5uMs8xkM" : address);
    allowed_token = ("KT1GCibU4MkPYzeuosuQpAymL73Wr8mHnQuv" : address);
    allowed_token_id = 0n;
  }
