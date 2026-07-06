(**************************************************************************)
(*  This file is part of the Codex semantics library                      *)
(*    (Union-find lattice subcomponent).                                  *)
(*                                                                        *)
(*  Copyright (C) 2026                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3.0                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

module PT = PatriciaTree

module ArrayWithCopy
    (Config : Parameters.ARRAY_CONFIG)
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP) =
struct
  type 'a node = 'a Node.t
  type ('a,'b) relation = ('a,'b) Relation.t

  let ( ** ) = Relation.compose
  let ( ~~ ) = Relation.inverse

  (** Parent pointers. Since we have no reverse mapping [int -> node],
      we must store [self]-nodes alongside the parents (needed in the join). *)
  type 'a ptr =
    | Uninitialized: 'a ptr
      (** Unitialized element. This will always be a element that never appeared in a union,
          and has no value. *)
    | Root: {
        self: 'a Node.t;
      } -> 'a ptr
    | Child: {
        self: 'a Node.t;
        relation: ('a,'b) Relation.t;
        parent: 'b Node.t;
      } -> 'a ptr

  (** [\exists 'a, 'a ptr] type. *)
  type wrapped = Wrap: 'a ptr -> wrapped [@@unboxed]

  type t = {
    parents: wrapped array;
    ranks: int array;
  }

  let default_rank = 0

  let make n = {
    parents = Array.make n (Wrap Uninitialized);
    ranks = Array.make n default_rank;
  }

  type 'a find_result = FindResult: {
    representative: 'b node;
    relation: ('a, 'b) relation;
  } -> 'a find_result

  (** Find in CPS style to avoid stack overflows *)
  let rec find: type a. t -> a node -> (a find_result -> 'res) -> 'res = fun uf x k ->
    let i = Node.to_int x in
    match uf.parents.(i) with
    | Wrap Uninitialized -> uf.parents.(i) <- Wrap (Root { self=x; });
                            k (FindResult { representative=x; relation=Relation.identity })
    | Wrap (Root _) -> k (FindResult { representative=x; relation=Relation.identity })
    | Wrap (Child c) ->
        find uf c.parent (fun (FindResult y) ->
            let relation = y.relation ** c.relation in
            match Node.polyeq c.self x with
            | Diff -> assert false
            | Eq ->
            if Config.path_compression = `Lazy then
              uf.parents.(i) <- Wrap(Child{ self=x; relation; parent=y.representative });
            k (FindResult{ y with relation }))
  let find uf x = find uf x Fun.id

  let check_related uf x y =
    let FindResult x = find uf x in
    let FindResult y = find uf y in
    match Node.polyeq x.representative y.representative with
    | Eq -> Some (~~ (y.relation) ** x.relation)
    | Diff -> None

  (** Helper for {!add_relation}, performs a directed union, making [child] point to [parent].
      Assumes [child] and [parent] are representatives *)
  let mk_child uf child parent relation rank =
    let child_ptr = Child { self=child; parent=parent; relation=relation } in
    uf.parents.(Node.to_int child) <- Wrap child_ptr;
    let parent_id = Node.to_int parent in
    begin match uf.parents.(parent_id) with
      | Wrap Uninitialized -> uf.parents.(parent_id) <- Wrap (Root {self=parent;})
      | _ -> () end;
    uf.ranks.(parent_id) <- rank;
    uf

  let combine_ranks small large = if small = large then large+1 else large

  let add_relation: type a b. t -> a Node.t -> b Node.t -> (a, b) Relation.t -> (t, (a, b) Relation.t) result =
    fun t a b rel ->
      let FindResult a = find t a in
      let FindResult b = find t b in
      match Node.polyeq a.representative b.representative with
      | Eq ->
          (* Both elements point to the same representative *)
          let old_rel = ~~(b.relation) ** a.relation in
          if Relation.equal rel old_rel then Ok t else Error old_rel
      | Diff ->
          let rank_a = t.ranks.(Node.to_int a.representative) in
          let rank_b = t.ranks.(Node.to_int b.representative) in
          if rank_a < rank_b
          then mk_child t a.representative b.representative
                        (b.relation ** rel ** ~~(a.relation))
                        (combine_ranks rank_a rank_b) |> Result.ok
          else mk_child t b.representative a.representative
                        (a.relation ** ~~rel ** ~~(b.relation))
                        (combine_ranks rank_b rank_a) |> Result.ok

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  let copy { parents; ranks } = { parents=Array.copy parents; ranks=Array.copy ranks }

  (** {2 Meet}                                                        *)
  (********************************************************************)

  type wrapped_relation =
    Rel: 'a node * ('a, 'b) relation * 'b node -> wrapped_relation

  let meet u v =
    let rec iter res n errs =
      if n < 0 then res, errs else
      match Array.get v.parents n with
      | Wrap(Uninitialized) | Wrap(Root _) -> iter res (n-1) errs
      | Wrap(Child{self;parent; relation}) ->
          match add_relation res self parent relation with
          | Ok res -> iter res (n-1) errs
          | Error _ -> iter res (n-1) (Rel(self, relation, parent)::errs)
    in iter (copy u) (Array.length u.parents - 1) []

  (** {2 Incl}                                                        *)
  (********************************************************************)

  let incl u v =
    let rec iter n =
      if n < 0 then true else
      match Array.get v.parents n with
      | Wrap(Uninitialized) | Wrap(Root _) -> iter (n-1)
      | Wrap(Child{self;parent;relation}) -> match check_related u self parent with
          | None -> false
          | Some rel -> Relation.equal relation rel && iter (n-1)
    in iter (Array.length u.parents - 1)


  module Triple = struct
    type _ t = Triple: 'a node * 'b node * ('a, 'b) relation -> ('a * 'b) t

    let polyeq: type a b. a t -> b t -> (a,b) PatriciaTree.cmp =
      fun (Triple (l1,r1,rel)) (Triple (l2,r2,rel')) ->
        match Node.polyeq l1 l2 with
        | Diff -> Diff
        | Eq -> match Node.polyeq r1 r2 with Eq -> if Relation.equal rel rel' then Eq else Diff | Diff -> Diff

    let hash: type a. a t -> int = fun (Triple (l,r,rel)) ->
      Utils.Functions.hash_pair (Node.to_int l) (Node.to_int r) |> Utils.Functions.hash_pair (Relation.hash rel)
  end

  type _ memoized_item = Item : {
    representative: 'c node;
    left_relation: ('a, 'c) relation;
  } -> ('a*'b) memoized_item

  module H = Utils.HetHashtbl.Make(Triple)(struct type ('a, _) t = 'a memoized_item end)

  type wnode = WNode: 'a Node.t -> wnode
  let get_node uf_a uf_b i =
    match uf_a.parents.(i) with
    | Wrap (Child { self; _; }) -> Some (WNode self)
    | Wrap (Root{ self; _ }) -> Some (WNode self)
    | Wrap Uninitialized ->
    match uf_b.parents.(i) with
    | Wrap (Child { self; _; }) -> Some (WNode self)
    | Wrap (Root{ self; _ }) -> Some (WNode self)
    | Wrap Uninitialized -> None


  let join uf_a uf_b =
    let n = Array.length uf_a.parents in
    let res = ref (make n) in
    let new_classes = H.create 10 in
    for i = 0 to n-1 do
      match get_node uf_a uf_b i with
      | None -> () (* uninitialized in both -> uninitialized in the join. *)
      | Some (WNode node) ->
      let FindResult a = find uf_a node in
      let FindResult b = find uf_b node in
      let triple = Triple.Triple(a.representative, b.representative, b.relation ** ~~(a.relation)) in
      match H.find_opt new_classes triple with
      | Some (Item i) ->
          res := add_relation !res node i.representative (i.left_relation ** a.relation) |> Result.get_ok
      | None ->
          H.add new_classes triple (Item{representative=node; left_relation= ~~(a.relation)});
    done; !res

  let check_invariants _ = None

  let pretty fmt t =
    Format.pp_print_list (fun fmt parent -> match parent with
      | Wrap Uninitialized -> Format.pp_print_string fmt "U"
      | Wrap (Root _) -> Format.fprintf fmt "R"
      | Wrap (Child c) -> Format.fprintf fmt "C(%a,%a)" Node.pretty c.parent Relation.pretty c.relation
    ) fmt (Array.to_list t.parents)
end


module PatriciaTree
    (Config : Parameters.PATRICIA_TREE_CONFIG)
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP) =
struct
  type 'a node = 'a Node.t
  type ('a,'b) relation = ('a,'b) Relation.t

  let copy x = x

  (** {2 Existential wrappers for the return type of find operations} *)

  type 'a find_result = FindResult: {
    representative: 'b node;
    relation: ('a, 'b) relation;
  } -> 'a find_result

  type 'a parent =
    | Child: { rank: int; parent: 'b node; relation: ('a, 'b) relation; } -> 'a parent
    | Root: { rank: int; } -> 'a parent

  module ReprMap = PatriciaTree.MakeHeterogeneousMap(Node)(struct type ('a, 'b) t = 'a parent end)
  (** Map [_ ReprMap.t] mapping ['a Node.t] to ['a parent] *)

  (** Union-find structure
      Values absent from the map implicitly point to themselves.
      However, non-trivial representatives MUST be present in the map for the
      {!join}.*)
  type t = {
    mutable parents: unit ReprMap.t;
    (** map: ['a Node.t --> ('b Node.t * ('a, 'b) relation)],
        mapping elements to representatives.
        representatives appear in this map's domain. *)
  }

  let ( ** ) = Relation.compose
  let ( ~~ ) = Relation.inverse

  let empty = { parents = ReprMap.empty; }

  let make _ = empty

  (** {2 Find operation} *)

  (** Find in CPS style to avoid stack overflows *)
  let rec find : type a. t -> a node -> (a find_result -> 'b) -> 'b = fun uf x k ->
    match ReprMap.find x uf.parents with
    | Child c -> find uf c.parent (fun (FindResult p) ->
                 let relation = p.relation ** c.relation in
                 (* path compression *)
                 if Config.path_compression = `Lazy then
                  begin match Node.polyeq p.representative c.parent with
                  | Eq -> ()
                  | Diff -> uf.parents <- ReprMap.add x (Child{c with parent=p.representative; relation}) uf.parents
                  end;
                  k (FindResult { representative=p.representative; relation }))
    | Root _ | exception Not_found -> k (FindResult { representative=x; relation=Relation.identity })
  let find uf x = find uf x Fun.id

  (** Variant of {!find} that also returns the value and the rank of the representative *)
  type ('a,'b) find_all_record = {
    representative: 'b node;
    relation: ('a, 'b) relation;
    rank: int
  }
  type 'a find_all =  FindAll: ('a,'b) find_all_record -> 'a find_all [@@unboxed]

  let default_rank = 0

  let rec find_all : type a. t -> a node -> (a find_all -> 'b) -> 'b = fun uf x k ->
    match ReprMap.find x uf.parents with
    | Child c -> find_all uf c.parent (fun (FindAll p) ->
                 let relation = p.relation ** c.relation in
                 (* path compression *)
                 if Config.path_compression = `Lazy then
                  begin match Node.polyeq p.representative c.parent with
                  | Eq -> ()
                  | Diff -> uf.parents <- ReprMap.add x (Child{ c with parent=p.representative; relation }) uf.parents
                  end;
                 k (FindAll { p with relation }))
    | Root r -> k (FindAll { representative=x; relation=Relation.identity; rank=r.rank; })
    | exception Not_found -> k (FindAll { representative=x; relation=Relation.identity; rank=default_rank; })
  let find_all uf x = find_all uf x Fun.id

  (** {2 Printers} *)

  let pretty fmt uf =
    if ReprMap.is_empty uf.parents
    then Format.fprintf fmt "Empty"
    else Format.fprintf fmt "@[%a@]"
        (ReprMap.pretty
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
            { f = fun fmt n p -> match p with
              | Root r -> Format.fprintf fmt "%a Root (%d)" Node.pretty n r.rank
              | Child c -> Format.fprintf fmt "%a -(%a)-> %a"
                           Node.pretty n Relation.pretty c.relation Node.pretty c.parent
            })
        uf.parents

  (** {2 Misc functions} *)

  let check_related uf a b =
    let FindResult a = find uf a in
    let FindResult b = find uf b in
    match Node.polyeq a.representative b.representative with
    | Eq -> Some(~~(b.relation) ** a.relation)
    | Diff -> None

  (** {2 Union operation} *)

  (** Returns the new rank, and a boolean signifying wether the rank has changed *)
  let combine_ranks small large = if small = large then large+1, true else large, false

  (** Helper for {!add_relation}, performs a directed union, making [child] point to [parent].
      Assumes [child] and [parent] are representatives *)
  let mk_child uf child parent relation (rank, rank_changed) =
    let parents = ReprMap.add child.representative (Child { parent=parent.representative; relation=relation; rank=child.rank }) uf.parents in
    (* We only need to write the parent if the value or the rank has changed *)
    if rank_changed
    then Ok { parents=ReprMap.add parent.representative (Root { rank }) parents }
    else Ok { parents }

  let add_relation: type a b. t -> a Node.t -> b Node.t -> (a, b) Relation.t -> (t, (a, b) Relation.t) result =
    fun t a b rel ->
      let FindAll a = find_all t a in
      let FindAll b = find_all t b in
      match Node.polyeq a.representative b.representative with
      | Eq ->
          (* Both elements point to the same representative *)
          let old_rel = ~~(b.relation) ** a.relation in
          if Relation.equal rel old_rel then Ok t else Error old_rel
      | Diff ->
          if (a.rank <= b.rank)
          then mk_child t a b (b.relation ** rel ** ~~(a.relation)) (combine_ranks a.rank b.rank)
          else mk_child t b a (a.relation ** ~~rel ** ~~(b.relation)) (combine_ranks b.rank a.rank)

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  type wrapped_relation =
    Rel: 'a node * ('a, 'b) relation * 'b node -> wrapped_relation

  let meet a b =
    ReprMap.fold_on_nonequal_union {f=fun x _ vb ((res, errs) as acc) ->
      match vb with
      | Some(Child{parent;relation;_}) -> begin match add_relation res x parent relation with
          | Ok res -> (res, errs)
          | Error _ -> (res, Rel(x, relation, parent) :: errs) end
      | _ -> acc
     } a.parents b.parents (a, [])

  let incl a b =
    ReprMap.reflexive_subset_domain_for_all2 {f=fun x vb _ ->
      match vb with
      | Child{parent;relation;_} -> begin match check_related a x parent with
          | None -> false
          | Some r -> Relation.equal r relation end
      | _ -> true
     } b.parents a.parents

  (** {2 Join operation}                                        *)
  (**************************************************************)
  (** {3 Join that calls union (from Section 3)}          *)
  (********************************************************)

  module Triple = struct
    type _ t = Triple: 'a node * 'b node * ('a, 'b) relation -> ('a * 'b) t

    let polyeq: type a b. a t -> b t -> (a,b) PatriciaTree.cmp =
      fun (Triple (l1,r1,rel)) (Triple (l2,r2,rel')) ->
        match Node.polyeq l1 l2 with
        | Diff -> Diff
        | Eq -> match Node.polyeq r1 r2 with Eq -> if Relation.equal rel rel' then Eq else Diff | Diff -> Diff

    let hash: type a. a t -> int = fun (Triple (l,r,rel)) ->
      Utils.Functions.hash_pair (Node.to_int l) (Node.to_int r) |> Utils.Functions.hash_pair (Relation.hash rel)
  end

  let rank = function
    | Root { rank; _ } -> rank
    | Child { rank; _ } -> rank

  type _ memoized_item = Item : {
    representative: 'c node;
    left_rel: ('a, 'c) relation; (* we need the left_rel to map new items to this candidate *)
    rank: int; incr_rank: bool;
  } -> ('a * 'b) memoized_item

  module H = Utils.HetHashtbl.Make(Triple)(struct type ('a, _) t = 'a memoized_item end)

  let memoized_get: type a. unit H.t -> a Triple.t -> int -> a memoized_item option =
    fun new_classes (Triple.Triple(l,r,rel) as triple) rank ->
    match Node.polyeq l r with
    | Diff -> H.find_opt new_classes triple
    | Eq ->
        if Relation.equal rel Relation.identity
        then Some (Item {representative=l; left_rel=Relation.identity; rank; incr_rank=false})
        else H.find_opt new_classes triple

  let join a b =
    (* map : repr_a -> repr_b -> list of repr_of_intersection for memoization *)
    let new_classes = H.create 10 in
    (* First loop: find the representative of the new class *)
    let new_classes = ReprMap.fold_on_nonequal_inter { f=fun (type a) (x: a node) va vb new_classes ->
      let FindAll pa = find_all a x in
      let FindAll pb = find_all b x in
      let repr_rank = min pa.rank pb.rank in
      let rank = min (rank va) (rank vb) in
      let triple = Triple.Triple(pa.representative, pb.representative, pb.relation ** ~~(pa.relation)) in
      begin match memoized_get new_classes triple repr_rank with
      | Some (Item candidate) ->
          if candidate.rank < rank
          then H.replace new_classes triple (Item { representative=x; left_rel= ~~(pa.relation); rank; incr_rank=false; })
          else if candidate.rank = rank && not candidate.incr_rank
          then H.replace new_classes triple (Item {candidate with incr_rank=true})
      | None -> H.add new_classes triple (Item { representative=x; left_rel= ~~(pa.relation); rank; incr_rank=false; })
      end; new_classes
    } a.parents b.parents new_classes in
    (* Second loop: compute the intersection *)
    let parents = ReprMap.idempotent_inter_filter { f=fun (type a) (x: a node) _ _ ->
      let FindAll pa = find_all a x in
      let FindAll pb = find_all b x in
      let repr_rank = min pa.rank pb.rank in
      let triple = Triple.Triple(pa.representative, pb.representative, pb.relation ** ~~(pa.relation)) in
      let Item i = memoized_get new_classes triple repr_rank |> Option.get in
      match Node.polyeq i.representative x with
      | Eq -> Some (Root { rank=i.rank + Bool.to_int i.incr_rank; })
      | Diff -> Some (Child {parent=i.representative; relation=i.left_rel**pa.relation; rank=min pa.rank pb.rank})
    } a.parents b.parents
    in { parents }

  (** {1 Debug operations}                                                    *)
  (****************************************************************************)

  let rec count_rank: type a. _ -> _ -> a node -> _ = fun t n i ->
    match ReprMap.find i t with
    | Root { rank; _ } -> (Format.asprintf "%a" Node.pretty i, rank, n)
    | Child c -> count_rank t (n+1) (c.parent)

  let check_invariants t =
    let errors = ReprMap.fold { f=fun k v errors ->
      match v with
      | Root _ -> errors
      | Child _ ->
          let root, rank, r = count_rank t.parents 0 k in
          if rank < r then
            (Format.asprintf "- path from %a to %s has length %d, but stored rank is %d" Node.pretty k root r rank::errors)
          else
            errors
      } t.parents [] in
    if errors = [] then None
    else
      let str = errors
                |> List.rev
                |> String.concat "\n" in
      Some ("Invalid ranks:\n" ^ str)
end


module PersistentArrayBase
    (PersistentArray : PersistentArray.S)
    (Config : Parameters.PERSISTENT_ARRAY_CONFIG)
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP)=
struct
  type 'a node = 'a Node.t
  type ('a,'b) relation = ('a,'b) Relation.t

  let copy x = x

  let ( ** ) = Relation.compose
  let ( ~~ ) = Relation.inverse

  (** The type of values store in our array. In classical union find these are
      just parent pointer, with representatives pointing to themselves. This version
      is a bit more complex:
      - We store values associated to each class at the {!Root}.
      - We store relation to the parent in {!Child} items.

      One other challenge is that array are indexed by integer, but we use {!Node.t}.
      One can get an integer from an [Node.t] by {!Node.to_int}, but there is no
      reverse mapping [int -> Node.t]. To that end:
      - we store self elements in {!Root} and {!Child}.
      - we have an {!Uninitialized} constructor for elements we have never seen. *)
  type 'a ptr =
    | Uninitialized: 'a ptr
      (** uninitialized element. This will always be a element that never appeared in a union,
          and has no value. *)
    | Root: {
        self: 'a Node.t;
      } -> 'a ptr
    | Child: {
        self: 'a Node.t;
        relation: ('a,'b) Relation.t;
        parent: 'b Node.t;
      } -> 'a ptr

  (** [\exists 'a, 'a ptr] type. *)
  type wrapped = Wrap: 'a ptr -> wrapped [@@unboxed]

  (** Unfortunately, there is no easy type safe way of remembering the ['a] type
      of each cell. *)
  type t = {
    mutable parents: wrapped PersistentArray.t;
    ranks: int PersistentArray.t;
  }

  (** {1 Array access}                                                        *)
  (****************************************************************************)
  (** For extendable arrays, all access must check for size, with set
      increasing the size if needed *)

  let get_fixed arr i ~default:_ = PersistentArray.get arr i
  let get_extendable arr i ~default =
    if i >= PersistentArray.size arr
    then default
    else PersistentArray.get arr i

  let set_fixed arr i ~default:_ = PersistentArray.set arr i
  let set_extendable arr i ~default v =
    let n = PersistentArray.size arr in
    if i >= n then PersistentArray.extend arr (max n (i+1-n)) default;
    PersistentArray.set arr i v

  let get = if Config.extendable then get_extendable else get_fixed
  let set = if Config.extendable then set_extendable else set_fixed

  (** {1 Union-find operations}                                               *)
  (****************************************************************************)

  let default_rank = 0

  let uninitialized = Wrap Uninitialized

  let make n = {
    parents=PersistentArray.make n uninitialized;
    ranks=PersistentArray.make n default_rank;
  }

  type 'a find_result = FindResult: {
    representative: 'b node;
    relation: ('a, 'b) relation;
  } -> 'a find_result

  (** CPS version of find, to avoid stack-overflows when performing deep-unions *)
  let rec find: type a. t -> a Node.t -> (a find_result -> 'b) -> 'b = fun uf node k ->
    let i = Node.to_int node in
    let Wrap(parent) = get ~default:uninitialized uf.parents i in
    match parent with
      | Uninitialized -> k (FindResult{representative=node; relation=Relation.identity})
      | Root r ->
          (* This match is only needed for type-checking, since node and r.self
             should always be equal. *)
          begin match Node.polyeq node r.self with
          | Eq -> k (FindResult{representative=r.self; relation=Relation.identity})
          | Diff -> failwith "Multiple nodes mapped to same to_int value"
          end
      | Child c ->
          (* This match is only needed for type-checking, since node and r.self
             should always be equal. *)
          match Node.polyeq node c.self with
          | Diff -> failwith "Multiple nodes mapped to same to_int value"
          | Eq ->
              find uf c.parent (fun (FindResult res) ->
                let rel = res.relation ** c.relation in
                (* path compression *)
                if Config.path_compression = `Lazy then
                  uf.parents <- set ~default:uninitialized uf.parents i (Wrap (Child {self=node; relation=rel; parent=res.representative }));
                k (FindResult { res with relation=rel }))
  let find t n = find t n Fun.id

  let check_related uf a b =
    let FindResult a = find uf a in
    let FindResult b = find uf b in
    match Node.polyeq a.representative b.representative with
    | Eq -> Some(~~(b.relation) ** a.relation)
    | Diff -> None

  (** {2 union operation}                                              *)
  (*********************************************************************)

  (** We return an option indicating wether or not the value has changed *)
  let combine_ranks small large = if small = large then Some (large+1) else None

  (** Helper for {!add_relation}, performs a directed union, making [child] point to [parent].
      Assumes [child] and [parent] are representatives *)
  let mk_child uf child parent relation rank =
    let child_ptr = Child { self=child; parent=parent; relation=relation } in
    let parents = set ~default:uninitialized uf.parents (Node.to_int child) (Wrap child_ptr) in
    (* We could do the union and then call [add_value uf child child_value],
       but inlining the call here removes a [PersistentArray.set]. *)
    let parent_id = Node.to_int parent in
    let parents =
      if get ~default:uninitialized uf.parents parent_id <> uninitialized
      then parents
      else set ~default:uninitialized parents parent_id (Wrap (Root { self=parent; } ))
    in Ok {
      parents;
      ranks = match rank with
        | None -> uf.ranks
        | Some rank -> set ~default:default_rank uf.ranks parent_id rank;
    }

  let add_relation: type a b. t -> a Node.t -> b Node.t -> (a, b) Relation.t -> (t, (a, b) Relation.t) result =
    fun t a b rel ->
      let FindResult a = find t a in
      let FindResult b = find t b in
      match Node.polyeq a.representative b.representative with
      | Eq ->
          (* Both elements point to the same representative *)
          let old_rel = ~~(b.relation) ** a.relation in
          if Relation.equal rel old_rel then Ok t else Error old_rel
      | Diff ->
          let rank_a = get ~default:default_rank t.ranks (Node.to_int a.representative) in
          let rank_b = get ~default:default_rank t.ranks (Node.to_int b.representative) in
          if (rank_a < rank_b)
          then mk_child t a.representative b.representative
                        (b.relation ** rel ** ~~(a.relation))
                        (combine_ranks rank_a rank_b)
          else mk_child t b.representative a.representative
                        (a.relation ** ~~rel ** ~~(b.relation))
                        (combine_ranks rank_b rank_a)

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  (** {2 Meet}                                                         *)
  (*********************************************************************)

  type wrapped_relation =
    Rel: 'a node * ('a, 'b) relation * 'b node -> wrapped_relation

  let meet u v =
    PersistentArray.diff_key v.parents u.parents
    |> fst
    |> Utils.Functions.list_of_hashtbl_keys
    |> List.filter_map (fun x ->
          match get ~default:uninitialized v.parents x with
          | Wrap(Child { self; parent; relation}) -> Some (Rel (self, relation, parent))
          | Wrap(Root _) | Wrap Uninitialized -> None)
    |> List.fold_left (fun (res, errs) elt -> match elt with
          | Rel(x,rel,px) as r -> match add_relation res x px rel with
              | Ok r -> r, errs
              | Error _ -> res, r::errs) (u, [])

  (** {2 Incl}                                                         *)
  (*********************************************************************)

  let incl u v =
    PersistentArray.diff_key v.parents u.parents
    |> fst
    |> Utils.Functions.list_of_hashtbl_keys
    |> List.filter_map (fun x ->
          match get ~default:uninitialized v.parents x with
          | Wrap(Child { self; parent; relation }) -> Some (Rel (self, relation, parent))
          | Wrap(Root _) | Wrap(Uninitialized) -> None)
    |> List.for_all (function Rel(x,rel,px) -> match check_related u x px with
          | None -> false
          | Some rel' -> Relation.equal rel rel')

  (** {2 Join}                                                         *)
  (*********************************************************************)

  module Triple = struct
    type _ t = Triple: 'a node * 'b node * ('a, 'b) relation -> ('a * 'b) t

    let polyeq: type a b. a t -> b t -> (a,b) PT.cmp =
      fun (Triple (l1,r1,rel)) (Triple (l2,r2,rel')) ->
        match Node.polyeq l1 l2 with
        | Diff -> Diff
        | Eq -> match Node.polyeq r1 r2 with Eq -> if Relation.equal rel rel' then Eq else Diff | Diff -> Diff

    let hash: type a. a t -> int = fun (Triple (l,r,rel)) ->
      Utils.Functions.hash_pair (Node.to_int l) (Node.to_int r) |> Utils.Functions.hash_pair (Relation.hash rel)
  end

  (** {3 Join by parent edits (Appendix A)}               *)
  (********************************************************)
  (** For this join, we need the ranks of both the current element and its representatives *)

  (** Unlike the paper, we can't simply run find on both sides, since do not know
      the node, only its integer id. Fortunately, the parents at that position should
      contain the node, if they are not uninitialized.
      They will not both be uninitialized since they appear in the diff, so they must be set,
      and we never set anything to uninitialized.

      Thus we have two special types: [find_left] which is returned by the find on
      the first argument, and [find_right] which is returned by the find on the
      second argument *)
  type find_left =
    | FL_NotFound of int
    | FL_Found : { id: int; node: 'a node; left: 'a find_result; rank: int; parent_rank:int } -> find_left

  type find_right = FindRight : {
    id: int;
    node: 'a node;
    left: 'a find_result;
    right: 'a find_result;
    rank: int; parent_rank: int;
  } -> find_right
  let find_left a id =
    (* id comes from the diff, so we can use get directly, no need to check in bounds *)
    match PersistentArray.get a.parents id with
    | Wrap Uninitialized -> FL_NotFound id
    | Wrap (Root x) ->
        (* the rank array can sometimes be smaller than parents, so it requires checks *)
        let rank = get ~default:default_rank a.ranks id in
        let left = FindResult {representative=x.self; relation=Relation.identity} in
        FL_Found{ id; node=x.self; left; rank; parent_rank=rank; }
    | Wrap (Child x) ->
        let (FindResult {representative; _} as left) = find a x.self in
        let rank = get ~default:default_rank a.ranks id in
        FL_Found{ id; node=x.self; left; rank;
          parent_rank=get ~default:default_rank a.ranks (Node.to_int representative); }

  let find_right b = function
    | FL_Found { id; node; rank; left; parent_rank; } ->
        let (FindResult {representative; _} as right) = find b node in FindRight{
          id; node; left; right;
          rank=min rank (get ~default:default_rank b.ranks id);
          parent_rank=min parent_rank (get ~default:default_rank b.ranks (Node.to_int representative));
        }
    | FL_NotFound id ->
    match PersistentArray.get b.parents id with
      | Wrap Uninitialized -> failwith "Unreachable"
      | Wrap (Root x) ->
          let repr = FindResult{representative=x.self; relation=Relation.identity; } in
          FindRight{
            id; node=x.self; rank=default_rank;
            left=repr; right=repr;
            parent_rank=default_rank;
          }
      | Wrap (Child x) ->
          let right = find b x.self in FindRight{
            id; node=x.self; rank=default_rank;
            left=FindResult{representative=x.self; relation=Relation.identity; }; right; parent_rank=default_rank;
          }

  type _ memoized_item = Item: {
    representative: 'c node;
    rank: int;
    incr_rank: bool;
    left_rel: ('a, 'c) relation;
  } -> ('a*'b) memoized_item
  module H = Utils.HetHashtbl.Make(Triple)(struct type ('a, _) t = 'a memoized_item end)

  let memoized_get: type a. unit H.t -> a Triple.t -> int -> a memoized_item option =
    fun new_classes (Triple.Triple(l,r,rel) as triple) rank ->
    match Node.polyeq l r with
    | Diff -> H.find_opt new_classes triple
    | Eq ->
        if Relation.equal rel Relation.identity
        then Some (Item {representative=l; left_rel=Relation.identity; rank; incr_rank=false})
        else H.find_opt new_classes triple

  (* First loop: find the representative of the new class *)
  let find_representatives new_classes (FindRight {node; left=FindResult left; right=FindResult right; rank; parent_rank;_ }) =
    let cross_rel = right.relation ** ~~(left.relation) in
    let triple = Triple.Triple(left.representative, right.representative, cross_rel) in
    (* lookup previous candidate for this pair, if no candidates and same repr, initialize with that repr *)
    match memoized_get new_classes triple parent_rank with
      | Some (Item candidate) ->
          if candidate.rank < rank
          then H.replace new_classes triple (Item { representative=node; left_rel= ~~(left.relation); rank; incr_rank=false; })
          else if candidate.rank = rank && not candidate.incr_rank
          then H.replace new_classes triple (Item {candidate with incr_rank=true})
      | None -> H.add new_classes triple (Item { representative=node; left_rel= ~~(left.relation); rank; incr_rank=false; })

  (* second loop body, update the arrays with the selected representatives *)
  let set_representatives new_classes (ranks, parents) (FindRight {id;node;left=FindResult left; right=FindResult right; rank; parent_rank;}) =
    let cross_rel = right.relation ** ~~(left.relation) in
    let triple = Triple.Triple(left.representative, right.representative, cross_rel) in
    let Item candidate = memoized_get new_classes triple parent_rank |> Option.get in
    match Node.polyeq candidate.representative node with
    | Diff -> (
        set ~default:default_rank ranks id rank,
        PersistentArray.set parents id (Wrap (Child {self=node; parent=candidate.representative; relation=candidate.left_rel ** left.relation}))
      )
    | Eq ->
        set ~default:default_rank ranks id (candidate.rank + Bool.to_int candidate.incr_rank),
        PersistentArray.set parents id (Wrap (Root {self=node;}))

  let join a b =
    let diff, ancestor = PersistentArray.diff_key a.parents b.parents in (* reroots PersistentArray at a *)
    let parents = match ancestor with Some a -> a | None -> b.parents in
    let diff_list =
      diff
      |> Utils.Functions.list_of_hashtbl_keys
      |> List.map (find_left a) (* using rev-map for tail recursion, since the order does not matter*)
      |> List.map (find_right b) in (* reroots PersistentArray at b *)
    let new_classes = H.create 100 in
    List.iter (find_representatives new_classes) diff_list;
    let ranks, parents = List.fold_left (set_representatives new_classes) (b.ranks, parents) diff_list in
    { ranks; parents }

  (** {1 Debug operations}                                                    *)
  (****************************************************************************)

  let pretty fmt t =
    PersistentArray.pretty (fun fmt parent -> match parent with
      | Wrap Uninitialized -> Format.pp_print_string fmt "U"
      | Wrap (Root r) -> Format.fprintf fmt "R(%a)" Node.pretty r.self
      | Wrap (Child c) -> Format.fprintf fmt "C(%a->%a,%a)" Node.pretty c.self Node.pretty c.parent Relation.pretty c.relation
    ) fmt t.parents

  let rec count_rank t n i =
    match PersistentArray.get t i with
    | Wrap (Root _) | Wrap Uninitialized -> (n,i)
    | Wrap (Child c) -> count_rank t (n+1) (Node.to_int c.parent)

  (** check that the class sizes are correct. *)
  let check_invariants t =
    let errors = Utils.Functions.range_fold (fun i errors ->
      let (rank, parent) = count_rank t.parents 0 i in
      let stored_rank = PersistentArray.get t.ranks parent in
      if (rank <= stored_rank)
      then errors
    else (Format.asprintf "- path from %d to %d has length %d, but stored rank is %d" i parent rank stored_rank)::errors)
      (PersistentArray.size t.parents - 1) [] in
    if errors = [] then None
    else
      let str = errors
                |> List.rev
                |> String.concat "\n" in
      Some ("Invalid ranks:\n" ^ str)
end

module PersistentArrayNCA = PersistentArrayBase(PersistentArray.Versioned)
module PersistentArray = PersistentArrayBase(PersistentArray)
