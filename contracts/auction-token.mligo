
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

(*
type token_metadata =
[@layout:comb]
{
  token_id : token_id;
  symbol : string;
  name : string;
  decimals : nat;
  extras : (string, string) map;
}
*)

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

(*
One of the options to make token metadata discoverable is to declare
`token_metadata : token_metadata_storage` field inside the FA2 contract storage
*)
type token_metadata_storage = (token_id, token_metadata) big_map


type fa2_entry_points =
  | Transfer of transfer list
  | Balance_of of balance_of_param
  | Update_operators of update_operator list
  (* | Token_metadata_registry of address contract *)

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

(* permissions descriptor entrypoint
type fa2_entry_points_custom =
  ...
  | Permissions_descriptor of permissions_descriptor contract

*)


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

(*
Entrypoints for sender/receiver hooks

type fa2_token_receiver =
  ...
  | Tokens_received of transfer_descriptor_param

type fa2_token_sender =
  ...
  | Tokens_sent of transfer_descriptor_param
*)

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

(*Only fails if admin is enabled and sender is not admin*)
let fail_if_not_admin (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith "NOT_AN_ADMIN"
        else unit
    | None -> unit

(*Only fails if admin is enabled and sender is not admin*)
let fail_if_not_admin_ext (storage, extra_msg : pauseable_admin_storage * string) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith ("NOT_AN_ADMIN" ^  "_"  ^ extra_msg)
        else unit
    | None -> unit

(*Only callable by admin*)
let set_admin (new_admin, storage : address * pauseable_admin_storage) : pauseable_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with pending_admin = Some new_admin; } : pauseable_admin_storage_record))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

(*Only callable by admin*)
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

type allowlist = unit

type allowlist_entrypoints = never

let init_allowlist : allowlist = unit

let check_single_token_allowed
    (addr, token_id, allowlist, err : address * token_id * allowlist * string) : unit =
  unit

type sale_id = nat

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

type pending_purchase =
  [@layout:comb]
  {
    sale_id : sale_id;
    purchaser : address;
  }


type pending_purchases = pending_purchase list

type permit =
  [@layout:comb]
  {
    signerKey: key;
    signature: signature;
  }

type permit_buy_param =
  [@layout:comb]
  {
    sale_id : sale_id;
    permit : permit;
  }

type offchain_bid_data =
  [@layout:comb]
  {
    asset_id : nat;
    bid_amount : tez;
  }

type permit_bid_param =
  [@layout:comb]
  {
    offchain_bid_data : offchain_bid_data;
    permit : permit;
  }

(*MATH*)

(*In English auction it is necessary to use ceiling so that bid is guaranteed to be raised*)
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

(*For fee calculations in Auction/Fixed-Price, normal division is used*)

let percent_of_price_tez (percent, price : nat * tez) : tez =
  ((price * percent)/ 100n)

let percent_of_price_nat (percent, price : nat * nat) : nat =
  ((price * percent)/ 100n)

(*HELPERS*)

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let tez_stuck_guard(entrypoint: string) : unit =
  let msg : string = "DONT_TRANSFER_TEZ_TO_" ^ entrypoint in
  (assert_msg(Tezos.amount = 0mutez, msg))

let address_to_contract_transfer_entrypoint(add : address) : ((transfer list) contract) =
  let c : (transfer list) contract option = Tezos.get_entrypoint_opt "%transfer" add in
  match c with
    None -> (failwith "ADDRESS_DOES_NOT_RESOLVE" : (transfer list) contract)
  | Some c ->  c

let resolve_contract (add : address) : unit contract =
  match ((Tezos.get_contract_opt add) : (unit contract) option) with
      None -> (failwith "ADDRESS_DOES_NOT_RESOLVE" : unit contract)
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

let transfer_tez (qty, to_ : tez * address) : operation =
  let destination : unit contract = resolve_contract (to_) in
  Tezos.transaction () qty destination

let check_tokens_allowed
    (tokens, allowlist, err : tokens * allowlist * string) : unit = unit

let address_from_key (key : key) : address =
  let a = Tezos.address (Tezos.implicit_account (Crypto.hash_key key)) in
  a

let check_permit (p, counter, param_hash : permit * nat * bytes) : unit =
    (* let unsigned : bytes = ([%Michelson ({| { SELF; ADDRESS; CHAIN_ID; PAIR; PAIR; PACK } |} : nat * bytes -> bytes)] (counter, param_hash) : bytes) in *)
    let unsigned : bytes = Bytes.pack ((Tezos.chain_id, Tezos.self_address), (counter, param_hash)) in
    let permit_valid : bool = Crypto.check p.signerKey p.signature unsigned in
    let u : unit = (if not permit_valid
     then ([%Michelson ({| { FAILWITH } |} : string * bytes -> unit)] ("MISSIGNED", unsigned) : unit)
     else ()) in
    u

type beneficiary =
[@layout:comb]
{
  address : address;
  percentage : nat;
}

type auction =
  [@layout:comb]
  {
    seller : address;
    current_bid : nat;
    start_time : timestamp;
    last_bid_time : timestamp;
    round_time : int;
    extend_time : int;
    asset : (tokens list);
    min_raise_percent : nat;
    min_raise : nat;
    end_time : timestamp;
    highest_bidder : address;
    royalty : nat;
    beneficiaries : beneficiary list;
  }

type configure_param =
  [@layout:comb]
  {
    opening_price : nat;
    min_raise_percent : nat;
    min_raise : nat;
    round_time : nat;
    extend_time : nat;
    asset : (tokens list);
    royalty : nat;
    beneficiaries : (beneficiary list);
    start_time : timestamp;
    end_time : timestamp;
  }

type royalty_fold =
[@layout:comb]
{
  ops: operation list;
  amount: nat;
  total: nat;
  total_percent: nat;
  token_address : address;
  token_id : nat;
}

type bid_param =
[@layout:comb]
{
  asset_id : nat;
  token_amount : nat;
}

type auction_without_configure_entrypoints =
  | Bid of bid_param
  | Cancel of nat
  | Resolve of nat
  | Admin of pauseable_admin
  | Set_commission of nat
  | Set_max_royalty of nat
  | Set_residual of address
  | Update_allowed of allowlist_entrypoints

type auction_entrypoints =
  | Configure of configure_param
  | AdminAndInteract of auction_without_configure_entrypoints

type storage =
  [@layout:comb]
  {
    admin : pauseable_admin_storage;
    commission : nat;
    max_royalty : nat;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    residual_address : address;
    auctions : (nat, auction) big_map;
    allowlist : allowlist;
    allowed_token : address;
    allowed_token_id : nat;
  }

type return = operation list * storage

let transfer_tokens_in_single_contract (from_ : address) (to_ : address) (tokens : tokens) : operation =
  let to_tx (fa2_tokens : fa2_tokens) : transfer_destination = {
      to_ = to_;
      token_id = fa2_tokens.token_id;
      amount = fa2_tokens.amount;
   } in
   let txs = List.map to_tx tokens.fa2_batch in
   let transfer_param = [{from_ = from_; txs = txs}] in
   let c = address_to_contract_transfer_entrypoint(tokens.fa2_address) in
   (Tezos.transaction transfer_param 0mutez c)

(*Handles transfers of tokens across FA2 Contracts*)
let transfer_tokens(tokens_list, from_, to_ : tokens list * address * address) : (operation list) =
   (List.map (transfer_tokens_in_single_contract from_ to_) tokens_list)

let rec tokens_list_to_operation_list_append (from_, to_, tokens_list, op_list : address * address * tokens list * (operation list)) :  (operation list) =
  let tokens = List.head_opt tokens_list in
  let new_tokens_list = List.tail_opt tokens_list in
  match tokens with
    | Some t ->
        let op = (transfer_tokens_in_single_contract from_ to_ t) in
        let new_op_list : operation list = (op :: op_list) in
        (match new_tokens_list with
          | Some tl -> tokens_list_to_operation_list_append(from_, to_, tl, new_op_list)
          | None -> (failwith "INTERNAL_ERROR" : operation list))
    | None -> op_list

let get_auction_data ((asset_id, storage) : nat * storage) : auction =
  match (Big_map.find_opt asset_id storage.auctions) with
      None -> (failwith "AUCTION_DOES_NOT_EXIST" : auction)
    | Some auction -> auction

let auction_ended (auction : auction) : bool =
  Tezos.now >= auction.end_time && Tezos.now > auction.last_bid_time + auction.round_time

let auction_started (auction : auction) : bool =
  Tezos.now >= auction.start_time

let auction_in_progress (auction : auction) : bool =
  auction_started(auction) && (not auction_ended(auction))

(*This condition is met iff no bid has been placed before the function executes*)
let first_bid (auction : auction) : bool =
  auction.highest_bidder = auction.seller

let dont_return_bid (auction : auction) : bool =
  first_bid(auction)

let valid_bid_amount (auction, bid_amount : auction * nat) : bool =
  (bid_amount >= (auction.current_bid + (percent_of_bid_nat (auction.min_raise_percent, auction.current_bid)))) ||
  (bid_amount >= auction.current_bid + auction.min_raise)                                            ||
  ((bid_amount >= auction.current_bid) && first_bid(auction))


let check_allowlisted (configure_param, allowlist : configure_param * allowlist) : unit = begin
  List.iter
    (fun (token : tokens) ->
      check_tokens_allowed (token, allowlist, "ASSET_NOT_ALLOWED")
    )
    configure_param.asset;
  unit end

let merge_ops (ops, op : operation list * operation) : operation list = op :: ops

let parse_beneficiaries (royalty, beneficiary : royalty_fold * beneficiary) : royalty_fold =
  let royalty_price : nat = royalty.amount * beneficiary.percentage / 10000n in
  let oplist = if royalty_price > 0n then transfer_fa2(royalty.token_address, royalty.token_id, royalty_price, Tezos.self_address, beneficiary.address) :: royalty.ops else royalty.ops in
  { royalty with
    ops = oplist;
    total = royalty.total + royalty_price;
    total_percent = royalty.total_percent + beneficiary.percentage;
  }

let configure_auction_storage(configure_param, seller, storage : configure_param * address * storage ) : storage = begin
    fail_if_paused storage.admin;
    check_allowlisted(configure_param, storage.allowlist);

    assert_msg (configure_param.end_time > configure_param.start_time, "INVALID_END_TIME");
    assert_msg (abs(configure_param.end_time - configure_param.start_time) <= storage.max_auction_time, "INVALID_AUCTION_TIME");

    assert_msg (configure_param.start_time >= Tezos.now, "INVALID_START_TIME");
    assert_msg (abs(configure_param.start_time - Tezos.now) <= storage.max_config_to_start_time, "MAX_CONFIG_TO_START_TIME_VIOLATED");

    assert_msg (configure_param.opening_price > 0n, "INVALID_OPENING_PRICE");
    tez_stuck_guard("CONFIGURE");
    assert_msg (configure_param.round_time > 0n, "INVALID_ROUND_TIME");
    assert_msg(configure_param.min_raise_percent > 0n && configure_param.min_raise > 0n, "INVALID_RAISE_CONFIGURATION");

    let royalty_fold : royalty_fold = {
      ops = ([] : operation list);
      amount = 0n;
      total = 0n;
      total_percent = 0n;
      token_address = storage.allowed_token;
      token_id = storage.allowed_token_id;
    } in
    let processed_royalties = List.fold parse_beneficiaries configure_param.beneficiaries royalty_fold in
    assert_msg (processed_royalties.total_percent = 10000n, "INVALID_BENEFICIARIES");

    let auction_data : auction = {
      seller = seller;
      royalty = configure_param.royalty;
      beneficiaries = configure_param.beneficiaries;
      current_bid = configure_param.opening_price;
      start_time = configure_param.start_time;
      round_time = int(configure_param.round_time);
      extend_time = int(configure_param.extend_time);
      asset = configure_param.asset;
      min_raise_percent = configure_param.min_raise_percent;
      min_raise = configure_param.min_raise;
      end_time = configure_param.end_time;
      highest_bidder = seller;
      last_bid_time = configure_param.start_time;
    } in
    let updated_auctions : (nat, auction) big_map = Big_map.update storage.current_id (Some auction_data) storage.auctions in
    {storage with auctions = updated_auctions; current_id = storage.current_id + 1n}
  end

let configure_auction(configure_param, storage : configure_param * storage) : return =
  let new_storage = configure_auction_storage(configure_param, Tezos.sender, storage) in
  let fa2_transfers : operation list = transfer_tokens(configure_param.asset, Tezos.sender, Tezos.self_address) in
  (fa2_transfers, new_storage)

let resolve_auction(asset_id, storage : nat * storage) : return = begin
    fail_if_paused storage.admin;
    tez_stuck_guard("RESOLVE");
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (auction_ended(auction) , "AUCTION_NOT_ENDED");

    let fa2_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.highest_bidder) in
    let oplist = if dont_return_bid(auction) then fa2_transfers else
      match storage.admin with
        | None ->
          let royalty_total : nat = if auction.royalty <> 0n then auction.current_bid * auction.royalty / 10000n else auction.current_bid in
          let seller_amt : nat = abs(auction.current_bid - royalty_total) in
          let royalty_fold : royalty_fold = {
            ops = ([] : operation list);
            amount = royalty_total;
            total = 0n;
            total_percent = 0n;
            token_address = storage.allowed_token;
            token_id = storage.allowed_token_id;
          } in
          let processed_royalties = List.fold parse_beneficiaries auction.beneficiaries royalty_fold in
          let royalty_rate : nat = processed_royalties.total / auction.current_bid * 10000n in
          let residual : nat = abs(auction.current_bid - processed_royalties.total - seller_amt) in
          let _u : unit = if auction.royalty <> 0n && royalty_rate > storage.max_royalty then (failwith "INVALID ROYALTY" : unit) else () in
          let _c : unit = if processed_royalties.total_percent <> 10000n then (failwith "INVALID BENEFICIARIES" : unit) else () in
          let ops = if seller_amt > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, seller_amt, Tezos.self_address, auction.seller) :: fa2_transfers else fa2_transfers in
          let ops_1 = if residual > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, residual, Tezos.self_address, storage.residual_address) :: ops else ops in
          List.fold merge_ops ops_1 processed_royalties.ops
        | Some admin ->
          let commission : nat = auction.current_bid * storage.commission / 10000n in
          let left : nat = abs(auction.current_bid - commission) in
          let royalty_total : nat = if auction.royalty <> 0n then auction.current_bid * auction.royalty / 10000n else left in
          let seller_amt : nat = abs(auction.current_bid - royalty_total - commission) in
          let royalty_fold : royalty_fold = {
            ops = ([] : operation list);
            amount = royalty_total;
            total = 0n;
            total_percent = 0n;
            token_address = storage.allowed_token;
            token_id = storage.allowed_token_id;
          } in
          let processed_royalties = List.fold parse_beneficiaries auction.beneficiaries royalty_fold in
          let royalty_rate : nat = processed_royalties.total / auction.current_bid * 10000n in
          let residual : nat = abs(auction.current_bid - commission - processed_royalties.total - seller_amt) in
          let _u : unit = if auction.royalty <> 0n && royalty_rate > storage.max_royalty then (failwith "INVALID ROYALTY" : unit) else () in
          let _c : unit = if processed_royalties.total_percent <> 10000n then (failwith "INVALID BENEFICIARIES" : unit) else () in
          let ops = if seller_amt > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, seller_amt, Tezos.self_address, auction.seller) :: fa2_transfers else fa2_transfers in
          let ops_1 = if commission > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, commission, Tezos.self_address, admin.admin) :: ops else ops in
          let ops_2 = if residual > 0n then transfer_fa2(storage.allowed_token, storage.allowed_token_id, residual, Tezos.self_address, storage.residual_address) :: ops_1 else ops_1 in
          List.fold merge_ops ops_2 processed_royalties.ops
      in

    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (oplist, {storage with auctions = updated_auctions})
    end

let cancel_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    let is_seller : bool = Tezos.sender = auction.seller in
    let v : unit = if is_seller then ()
          else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
    assert_msg (not auction_ended(auction), "AUCTION_ENDED");
    tez_stuck_guard("CANCEL");

    let fa2_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.seller) in
    let op_list : operation list = if dont_return_bid(auction) then
      fa2_transfers else
      let return_bid = transfer_fa2(storage.allowed_token, storage.allowed_token_id, auction.current_bid, Tezos.self_address, auction.highest_bidder) in
      (return_bid :: fa2_transfers) in
    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (op_list, {storage with auctions = updated_auctions})
  end

let place_bid(  asset_id
              , auction
              , bid_amount
              , bidder
              , storage
              : nat
              * auction
              * nat
              * address
              * storage
             ) : return = begin

    assert_msg (Tezos.sender = Tezos.source, "CALLER_NOT_IMPLICIT");
    (fail_if_paused storage.admin);
    assert_msg (auction_in_progress(auction), "NOT_IN_PROGRESS");
    assert_msg(bidder <> auction.seller, "SEllER_CANT_BID");
    assert_msg(bidder <> auction.highest_bidder, "NO_SELF_OUTBIDS");
    (if not valid_bid_amount(auction, bid_amount)
      then ([%Michelson ({| { FAILWITH } |} : string * (nat * nat * address * timestamp * timestamp) -> unit)] ("INVALID_BID_AMOUNT", (auction.current_bid, bid_amount, auction.highest_bidder, auction.last_bid_time, Tezos.now)) : unit)
      else ());
    let op_list : operation list = if dont_return_bid(auction) then
      ([] : operation list) else
      let return_bid = transfer_fa2(storage.allowed_token, storage.allowed_token_id, auction.current_bid, Tezos.self_address, auction.highest_bidder) in
      [return_bid] in
    let new_end_time = if auction.end_time - Tezos.now <= auction.extend_time then
      Tezos.now + auction.extend_time else auction.end_time in
    let updated_auction_data = {auction with current_bid = bid_amount; highest_bidder = bidder;
                                last_bid_time = Tezos.now; end_time = new_end_time;
                               } in
    let updated_auctions = Big_map.update asset_id (Some updated_auction_data) storage.auctions in
    (op_list , {storage with auctions = updated_auctions})
  end

let place_bid_onchain(bid_param, storage : bid_param * storage) : return = begin
    let bid_amount = bid_param.token_amount in
    let asset_id = bid_param.asset_id in
    let bidder = Tezos.sender in
    let auction : auction = get_auction_data(asset_id, storage) in
    let bid_placed_offchain : bool = false in
    let tx_fa2 = transfer_fa2(storage.allowed_token, storage.allowed_token_id, bid_amount, bidder, Tezos.self_address) in
    let (ops, new_storage) = place_bid(asset_id, auction, bid_amount, bidder, storage) in
    (tx_fa2 :: ops, new_storage)
  end

let admin(admin_param, storage : pauseable_admin * storage) : return =
    let ops, admin = pauseable_admin(admin_param, storage.admin) in
    let new_storage = { storage with admin = admin; } in
    ops, new_storage

let update_allowed(allowlist_param, storage : allowlist_entrypoints * storage) : return =
    [%Michelson ({| { NEVER } |} : never -> return)] allowlist_param

let set_commission (commission, storage : nat * storage) : operation list * storage =
    ([] : operation list), { storage with commission = commission; }

let set_royalty (royalty, storage : nat * storage) : operation list * storage =
    ([] : operation list), { storage with max_royalty = royalty; }

let set_residual (addr, storage : address * storage) : operation list * storage =
    ([] : operation list), { storage with residual_address = addr; }

let english_auction_tez_no_configure (p,storage : auction_without_configure_entrypoints * storage) : return =
  match p with
    | Bid bid -> place_bid_onchain(bid, storage)
    | Cancel asset_id -> cancel_auction(asset_id, storage)
    | Resolve asset_id -> resolve_auction(asset_id, storage)
    | Set_commission commission ->
        let _u : unit = fail_if_not_admin(storage.admin) in
        set_commission(commission, storage)
    | Set_max_royalty royalty ->
        let _u : unit = fail_if_not_admin(storage.admin) in
        set_royalty(royalty, storage)
    | Set_residual addr ->
        let _u : unit = fail_if_not_admin(storage.admin) in
        set_residual(addr, storage)
    | Admin a -> admin(a, storage)
    | Update_allowed a -> update_allowed(a, storage)

let english_auction_tez_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | AdminAndInteract ai -> english_auction_tez_no_configure(ai, storage)

let sample_storage : storage = {
  admin = Some ({
    admin = ("tz1TkJxyXXj5DtrmKvvNc1eqcEPV5uMs8xkM" : address);
    paused = false;
    pending_admin = (None : address option);
  });
  commission = 639n;
  max_royalty = 2000n;
  current_id = 0n;
  max_auction_time = 86400n;
  max_config_to_start_time = 3600n;
  residual_address = ("tz1TkJxyXXj5DtrmKvvNc1eqcEPV5uMs8xkM" : address);
  auctions = (Big_map.empty : (nat, auction) big_map);
  allowlist = unit;
  allowed_token = ("KT1GCibU4MkPYzeuosuQpAymL73Wr8mHnQuv" : address);
  allowed_token_id = 0n;
}
