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

module ArrayWithCopy
    (Config: Parameters.ARRAY_CONFIG)
    (Node: Parameters.NODE_WITH_COMPARE) =
struct

  (** {1 Core type}                                                           *)
  (****************************************************************************)

  type node = Node.t

  (** The type of values store in our array.
      One can get an integer from an [Node.t] by {!Node.to_int}, but there is no
      reverse mapping [int -> Node.t]. To that end:
      - we store self elements in {!Root} and {!Child}.
      - we have an {!Uninitialized} constructor for elements we have never seen. *)
  type ptr =
    | Uninitialized
      (** Unitialized element. This will always be a element that never appeared in a union,
          and has no value. *)
    | Root of {
        self: Node.t;
      }
    | Child of {
        self: Node.t;
        parent: Node.t;
      }

  (** The main UF type *)
  type t = {
    parents: ptr array;
    ranks: int array; (** for union-by-size, this is the array of sizes. *)
  }

  let copy x = { parents=Array.copy x.parents; ranks=Array.copy x.ranks; }

  (** {1 Union-find operations}                                               *)
  (****************************************************************************)

  let default_rank = 0

  let make n = {
    parents=Array.make n (Uninitialized);
    ranks=Array.make n default_rank;
  }

  (** tail-recusrive, CPS implementation of find with path compression *)
  let rec find_pc uf node k =
    let i = Node.to_int node in
    match Array.get uf.parents i with
      | Uninitialized | Root _ -> k node
      | Child c -> find_pc uf c.parent (fun p ->
          Array.set uf.parents i (Child { self=node; parent=p });
          k p)
  let find_pc uf node = find_pc uf node Fun.id

  let rec find_npc uf node =
    let i = Node.to_int node in
    match Array.get uf.parents i with
    | Uninitialized | Root _ -> node
    | Child c -> find_npc uf c.parent

  let find = match Config.path_compression with `Lazy -> find_pc | `None -> find_npc

  let check_related uf a b = Node.equal (find uf a) (find uf b)

  (** Helper for {!union}, performs a directed union, making [child] point to [parent].
      Assumes [child] and [parent] are representatives *)
  let mk_child uf child parent rank =
    let child_ptr = Child { self=child; parent=parent; } in
    Array.set uf.parents (Node.to_int child) child_ptr;
    let parent_id = Node.to_int parent in
    let () = match Array.get uf.parents parent_id with
      | Uninitialized -> Array.set uf.parents parent_id (Root { self=parent; })
      | _ -> () in
    Array.set uf.ranks parent_id rank;
    uf

  let combine_ranks small large = if small = large then large+1 else large
  let union t a b =
    let repr_a = find t a in
    let repr_b = find t b in
    if Node.equal repr_a repr_b then t else
    let rank_a = Array.get t.ranks (Node.to_int repr_a) in
    let rank_b = Array.get t.ranks (Node.to_int repr_b) in
    if (rank_a < rank_b)
    then mk_child t repr_a repr_b (combine_ranks rank_a rank_b)
    else mk_child t repr_b repr_a (combine_ranks rank_b rank_a)

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  (** {2 Meet}                                                        *)
  (********************************************************************)

  let meet u v =
    let rec iter res n =
      if n < 0 then res else
      match Array.get v.parents n with
      | Uninitialized | Root _-> iter res (n-1)
      | Child{self;parent} -> iter (union res self parent) (n-1)
    in iter (copy u) (Array.length u.parents - 1)

  (** {2 Incl}                                                        *)
  (********************************************************************)

  let incl u v =
    let rec iter n =
      if n < 0 then true else
      match Array.get v.parents n with
      | Uninitialized | Root _-> iter (n-1)
      | Child{self;parent} -> check_related u self parent && iter (n-1)
    in iter (Array.length u.parents - 1)

  (** {2 Join}                                                         *)
  (*********************************************************************)

  module Pair = struct
    type t = node * node
    let equal (l1, r1) (l2, r2) = Node.equal l1 l2 && Node.equal r1 r2
    let hash (l, r) =
      Utils.Functions.hash_pair (Node.to_int l) (Node.to_int r)
  end

  module H = Hashtbl.Make(Pair)

  let join u v =
    let n = Array.length u.parents in
    let res = make n in
    let new_classes = H.create 100 in
    for k = 0 to n - 1 do
      let k = Node.of_int k in
      let pair = find u k, find v k in
      match H.find_opt new_classes pair with
      | Some x -> ignore (union res k x)
      | None -> H.add new_classes pair k
    done; res

  (** {1 Debug operations}                                                    *)
  (****************************************************************************)

  let pretty fmt t =
    PersistentArray.pretty (fun fmt parent -> match parent with
      | Uninitialized -> Format.pp_print_string fmt "U"
      | Root _ -> Format.fprintf fmt "R"
      | Child c -> Format.fprintf fmt "C(%a)" Node.pretty c.parent
    ) fmt (PersistentArray.of_array t.parents)

  let rec count_rank t n i =
    match Array.get t i with
    | Root _ | Uninitialized -> (n,i)
    | Child c -> count_rank t (n+1) (Node.to_int c.parent)

  (** check that the class sizes are correct. *)
  let check_invariants t =
    let errors = Utils.Functions.range_fold (fun i errors ->
      let (rank, parent) = count_rank t.parents 0 i in
      let stored_rank = Array.get t.ranks parent in
      if (rank <= stored_rank)
      then errors
    else (Format.asprintf "- path from %d to %d has length %d, but stored rank is %d" i parent rank stored_rank)::errors)
      (Array.length t.parents - 1) [] in
    if errors = [] then None else
    let str = errors |> List.rev |> String.concat "\n" in
    Some ("Invalid ranks:\n" ^ str)
end


module PatriciaTree
    (Config : Parameters.PATRICIA_TREE_CONFIG)
    (Node : Parameters.NODE) =
struct

  (** {1 Core type}                                                           *)
  (****************************************************************************)

  type node = Node.t

  type parent =
    | Child of { rank: int; parent: node; }
    | Root of { rank: int; }

  module ReprMap = PatriciaTree.MakeMap(Node)
  (** Map [_ ReprMap.t] mapping ['a Node.t] to ['a parent] *)

  (** Union-find structure
      Values absent from the map implicitly point to themselves.
      However, non-trivial representatives MUST be present in the map for the
      {!join}. *)
  type t = {
    mutable parents: parent ReprMap.t;
    (** mutable for path compression *)
  }

  (** {1 Union-find operations}                                               *)
  (****************************************************************************)

  let empty = { parents = ReprMap.empty; }

  let make _ = empty

  (** {2 Find operation}                                               *)
  (*********************************************************************)

  (** Without path compression *)
  let rec find_npc uf x =
    match ReprMap.find x uf.parents with
    | Child c -> find_npc uf c.parent
    | Root _ | exception Not_found -> x

  (** With path compression, in CPS style to be tail-recursive *)
  let rec find_pc uf x k =
    match ReprMap.find x uf.parents with
    | Root _ | exception Not_found -> k x
    | Child c -> find_pc uf c.parent (fun p ->
        if not (Node.equal c.parent p) then
          uf.parents <- ReprMap.add x (Child { c with parent = p}) uf.parents;
        k p)
  let find_pc uf x = find_pc uf x Fun.id

  let find = match Config.path_compression with `Lazy -> find_pc | `None -> find_npc

  let default_rank = 0

  (** Variant of find that also returns rank, without path compression *)
  let rec find_rank_npc uf x =
    match ReprMap.find x uf.parents with
    | Child c -> find_rank_npc uf c.parent
    | Root { rank } -> x, rank
    | exception Not_found -> x, default_rank

  (** Variant of find that also returns rank, with path compression *)
  let rec find_rank_pc uf x k =
    match ReprMap.find x uf.parents with
    | Root { rank } -> k (x, rank)
    | exception Not_found -> k (x, default_rank)
    | Child c -> find_rank_pc uf c.parent (fun ((p,_) as res) ->
        if not (Node.equal c.parent p) then
          uf.parents <- ReprMap.add x (Child { c with parent = p}) uf.parents;
        k res)
  let find_rank_pc uf x = find_rank_pc uf x Fun.id

  let find_rank = match Config.path_compression with `Lazy -> find_rank_pc | `None -> find_rank_npc

  let check_related uf a b = Node.equal (find uf a) (find uf b)

  (** {2 union operation}                                              *)
  (*********************************************************************)

  let combine_ranks small large = if small = large then large+1 else large

  (** Helper for {!union}, performs a directed union, making [child] point to [parent].
      Assumes [child] and [parent] are representatives *)
  let mk_child uf child child_rank parent parent_rank =
    let parents = ReprMap.add child (Child { parent=parent; rank=child_rank }) uf.parents in
    (* We only need to write the parent if the value or the rank has changed,
      conveniently, this will always insert a parent if it was absent (rank of 0 equal to child rank) *)
    let parent_rank' = combine_ranks child_rank parent_rank in
    if parent_rank' = parent_rank
    then { parents }
    else { parents=ReprMap.add parent (Root { rank=combine_ranks child_rank parent_rank' }) parents }

  let union t a b =
      let pa, ra = find_rank t a in
      let pb, rb = find_rank t b in
      match Node.equal pa pb with
      | true -> t
      | false ->
          if ra <= rb
          then mk_child t pa ra pb rb
          else mk_child t pb rb pa ra

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  let meet a b =
    ReprMap.fold_on_nonequal_union (fun x _ vb res ->
      match vb with
      | Some(Child{parent;_}) -> union res x parent
      | _ -> res
      ) a.parents b.parents a

  let incl a b =
    ReprMap.reflexive_subset_domain_for_all2 (fun x vb _ ->
      match vb with
      | Child{parent;_} -> check_related a x parent
      | _ -> true
      ) b.parents a.parents

  (** {2 Join operation}                                        *)
  (**************************************************************)

  module Pair = struct
    type t = node * node
    let equal (l1, r1) (l2, r2) = Node.equal l1 l2 && Node.equal r1 r2
    let hash (l,r) = Utils.Functions.hash_pair (Node.to_int l) (Node.to_int r)
  end

  module H = Hashtbl.Make(Pair)
  let rank = function
    | Root { rank; _ } -> rank
    | Child { rank; _ } -> rank

  (** {3 Join by parent edits (Appendix A)}               *)
  (********************************************************)

  type memoized_item = {
    representative: node;
    rank: int;
    incr_rank: bool;
  }

  let memoized_get new_classes ((l,r) as pair) rank =
    match Node.equal l r with
    | false -> H.find_opt new_classes pair
    | true -> Some {representative=l; rank; incr_rank=false}

  let join a b =
    (* map : repr_a -> repr_b -> list of repr_of_intersection for memoization *)
    let new_classes = H.create 10 in
    (* First loop: find the representative of the new class *)
    let new_classes = ReprMap.fold_on_nonequal_inter (fun x va vb new_classes ->
      let pa, ra = find_rank a x in
      let pb, rb = find_rank b x in
      let rank = min (rank va) (rank vb) in
      let pair = pa, pb in
      begin match memoized_get new_classes pair (min ra rb) with
      | Some candidate ->
          if candidate.rank < rank
          then H.replace new_classes pair { representative=x; rank; incr_rank=false; }
          else if candidate.rank = rank && not candidate.incr_rank
          then H.replace new_classes pair { candidate with incr_rank=true}
      | None -> H.add new_classes pair { representative=x; rank; incr_rank=false; }
      end; new_classes
     ) a.parents b.parents new_classes in
    (* Second loop: compute the intersection *)
    let parents = ReprMap.idempotent_inter_filter (fun x va vb ->
      let pa, ra = find_rank a x in
      let pb, rb = find_rank b x in
      let i = memoized_get new_classes (pa, pb) (min ra rb) |> Option.get in
      match Node.equal i.representative x with
      | true -> let rank = i.rank + Bool.to_int i.incr_rank in if rank=0 then None else Some (Root { rank })
      | false -> Some (Child {parent=i.representative; rank=min (rank va) (rank vb) })
     ) a.parents b.parents
    in { parents }

  (** {1 Debug operations}                                                    *)
  (****************************************************************************)

  let copy x = x

  let pretty fmt uf =
    if ReprMap.is_empty uf.parents
    then Format.fprintf fmt "Empty"
    else Format.fprintf fmt "@[%a@]"
        (ReprMap.pretty
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
            (fun fmt n p -> match p with
              | Root r -> Format.fprintf fmt "%a Root (%d)" Node.pretty n r.rank
              | Child c -> Format.fprintf fmt "%a --> %a"
                           Node.pretty n Node.pretty c.parent
            ))
        uf.parents

  let rec count_rank t n i =
    match ReprMap.find i t with
    | Root { rank; _ } -> (Format.asprintf "%a" Node.pretty i, rank, n)
    | Child c -> count_rank t (n+1) (c.parent)

  let check_invariants t =
    let errors = ReprMap.fold (fun k v errors ->
      match v with
      | Root _ -> errors
      | Child _ ->
          let root, rank, r = count_rank t.parents 0 k in
          if rank < r then
            (Format.asprintf "- path from %a to %s has length %d, but stored rank is %d" Node.pretty k root r rank::errors)
          else
            errors
     ) t.parents [] in
    if errors = [] then None
    else
      let str = errors
                |> List.rev
                |> String.concat "\n" in
      Some ("Invalid ranks:\n" ^ str)
end


module PersistentArrayBase
    (PersistentArray: PersistentArray.S)
    (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
    (Node : Parameters.NODE) =
struct

  (** {1 Core type}                                                           *)
  (****************************************************************************)

  type node = Node.t

  (** The type of values store in our array.
      One can get an integer from an [Node.t] by {!Node.to_int}, but there is no
      reverse mapping [int -> Node.t]. To that end:
      - we store self elements in {!Root} and {!Child}.
      - we have an {!Uninitialized} constructor for elements we have never seen. *)
  type ptr =
    | Uninitialized
      (** Unitialized element. This will always be a element that never appeared in a union,
          and has no value. *)
    | Root of {
        self: Node.t;
      }
    | Child of {
        self: Node.t;
        parent: Node.t;
      }

  type t = {
    mutable parents: ptr PersistentArray.t; (* mutable for path compression *)
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

  let make n = {
    parents=PersistentArray.make n Uninitialized;
    ranks=PersistentArray.make n default_rank;
  }

  (** {2 Find operation}                                               *)
  (*********************************************************************)

  (** Without path compression *)
  let rec find_npc uf node =
    let i = Node.to_int node in
    match get ~default:Uninitialized uf.parents i with
      | Uninitialized | Root _ -> node
      | Child c -> find_npc uf c.parent

  (** With path compression, in CPS style to be tail-recursive *)
  let rec find_pc uf node k =
    let i = Node.to_int node in
    match get ~default:Uninitialized uf.parents i with
      | Uninitialized | Root _ -> k node
      | Child c -> find_pc uf c.parent (fun p ->
          uf.parents <- set ~default:Uninitialized uf.parents i (Child { self=node; parent=p });
          k p)
  let find_pc uf node = find_pc uf node Fun.id

  let find = match Config.path_compression with `Lazy -> find_pc | `None -> find_npc

  let check_related uf a b = Node.equal (find uf a) (find uf b)

  (** {2 union operation}                                              *)
  (*********************************************************************)

  (** We return an option indicating wether or not the value has changed *)
  let combine_ranks small large = if small = large then Some (large+1) else None

  (** Helper for {!union}, performs a directed union, making [child] point to [parent].
      Assumes [child] and [parent] are representatives *)
  let mk_child uf child parent rank =
    let child_ptr = Child { self=child; parent=parent; } in
    let parents = set ~default:Uninitialized uf.parents (Node.to_int child) child_ptr in
    let parent_id = Node.to_int parent in
    let parents = match get ~default:Uninitialized parents parent_id with
      | Uninitialized -> set ~default:Uninitialized parents parent_id (Root { self=parent; })
      | _ -> parents in {
      parents;
      ranks = match rank with
        | None -> uf.ranks
        | Some rank -> set ~default:default_rank uf.ranks parent_id rank;
    }

  let union t a b =
    let repr_a = find t a in
    let repr_b = find t b in
    if Node.equal repr_a repr_b then t else
    let rank_a = get ~default:default_rank t.ranks (Node.to_int repr_a) in
    let rank_b = get ~default:default_rank t.ranks (Node.to_int repr_b) in
    if (rank_a < rank_b)
    then mk_child t repr_a repr_b (combine_ranks rank_a rank_b)
    else mk_child t repr_b repr_a (combine_ranks rank_b rank_a)

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  (** {2 Meet}                                                         *)
  (*********************************************************************)

  let node_of_ptr = function
    | Uninitialized -> None
    | Root { self; _ } -> Some self
    | Child { self; _} -> Some self
  let node_of_ptr_parent = function
    | Uninitialized -> None
    | Root { self; _ } -> Some self
    | Child { parent; _} -> Some parent

  let meet u v =
    PersistentArray.diff_key u.parents v.parents
    |> fst
    |> Utils.Functions.list_of_hashtbl_keys
    |> List.map (fun x ->
          let ptr = get ~default:Uninitialized u.parents x in
          (node_of_ptr ptr, node_of_ptr_parent ptr))
    |> List.filter_map (function
      | _, None -> None (* parent is unitialized -> self repr -> trivial union *)
      | None, Some _ -> failwith "Unreachable"
      | Some x, Some px -> Some (x, px))
    |> List.fold_left (fun res (x, px) -> union res x px) v

  (** {2 Incl}                                                         *)
  (*********************************************************************)

  let incl u v =
    PersistentArray.diff_key v.parents u.parents
    |> fst
    |> Utils.Functions.list_of_hashtbl_keys
    |> List.map (fun x ->
          let ptr = get ~default:Uninitialized v.parents x in
          (node_of_ptr ptr, node_of_ptr_parent ptr))
    |> List.filter_map (function
      | _, None -> None (* parent is unitialized -> self repr -> trivial union *)
      | None, Some _ -> failwith "Unreachable"
      | Some x, Some px -> Some (x, px))
    |> List.for_all (fun (x, px) -> check_related u x px)



  (** {2 Join}                                                         *)
  (*********************************************************************)

  module Pair = struct
    type t = node * node
    let equal (l1, r1) (l2, r2) = Node.equal l1 l2 && Node.equal r1 r2
    let hash (l, r) =
      Utils.Functions.hash_pair (Node.to_int l) (Node.to_int r)
  end

  module H = Hashtbl.Make(Pair)

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
    | FL_Found of {
        id: int;
        node: node;
        rank: int;
        left_repr: node;
        parent_rank: int;
    }
  let find_left a id =
    (* id comes from the diff, so we can use get directly, no need to check in bounds *)
    match PersistentArray.get a.parents id with
    | Uninitialized -> FL_NotFound id
    | Root x ->
        (* the rank array can sometimes be smaller than parents, so it requires checks *)
        let rank = get ~default:default_rank a.ranks id in
        FL_Found{
          id; node=x.self;
          left_repr=x.self;
          rank; parent_rank=rank;
        }
    | Child x ->
        let left_repr = find a x.self in
        FL_Found{ id; node=x.self; left_repr;
          rank=get ~default:default_rank a.ranks id;
          parent_rank=get ~default:default_rank a.ranks (Node.to_int left_repr)
        }

  type find_right = {
    id: int;
    node: node;
    rank: int;
    left_repr: node;
    right_repr: node;
    parent_rank: int;
  }
  let find_right b = function
    | FL_Found { id; node; rank; left_repr;  parent_rank } ->
        let right_repr = find b node in {
          id; node; left_repr; right_repr;
          rank=min rank (get ~default:default_rank b.ranks id);
          parent_rank=min parent_rank (get ~default:default_rank b.ranks (Node.to_int right_repr))
        }
    | FL_NotFound id ->
    match PersistentArray.get b.parents id with
      | Uninitialized -> failwith "Unreachable"
      | Root x -> {
            id; node=x.self; rank=default_rank;
            left_repr=x.self;
            right_repr=x.self;
            parent_rank=default_rank;
          }
      | Child x ->
          let right_repr = find b x.self in {
            id; node=x.self; rank=default_rank;
            left_repr=x.self;
            right_repr; parent_rank=default_rank;
          }

  type memoized_item =  {
    representative: node;
    rank: int;
    incr_rank: bool;
  }

  let memoized_get new_classes ((l,r) as pair) rank =
    if Node.equal l r
    then Some {representative=l; rank; incr_rank=false}
    else H.find_opt new_classes pair

  (* First loop: find the representative of the new class *)
  let find_representatives new_classes { node; left_repr; right_repr; rank; _ } =
      let pair = (left_repr, right_repr) in
      (* lookup previous candidate for this pair, if no candidates and same repr, initialize with that repr *)
      match memoized_get new_classes pair rank with
      | Some candidate ->
          if candidate.rank < rank
          then H.replace new_classes pair { representative=node; rank; incr_rank=false; }
          else if candidate.rank = rank && not candidate.incr_rank
          then H.replace new_classes pair ({candidate with incr_rank=true})
      | None -> H.add new_classes pair { representative=node; rank; incr_rank=false; }

  (* second loop body, update the arrays with the selected representatives *)
  let set_representatives new_classes (ranks, parents) {id;node;left_repr; right_repr; rank; parent_rank;} =
    let pair = (left_repr, right_repr)  in
    let candidate = memoized_get new_classes pair parent_rank |> Option.get in
    match Node.equal candidate.representative node with
    | false -> (
        set ~default:default_rank ranks id rank,
        PersistentArray.set parents id (Child {self=node; parent=candidate.representative;})
      )
    | true ->
        set ~default:default_rank ranks id (candidate.rank + Bool.to_int candidate.incr_rank),
        PersistentArray.set parents id (Root {self=node;})

  let join a b =
    let diff, ancestor = PersistentArray.diff_key a.parents b.parents in (* reroots PersistentArray at a *)
    let parents = match ancestor with Some a -> a | None -> b.parents in
    let diff_list = diff
      |> Utils.Functions.list_of_hashtbl_keys
      |> List.map (find_left a) (* using rev-map for tail recursion, since the order does not matter*)
      |> List.map (find_right b) in (* reroots PersistentArray at b *)
    let new_classes = H.create 100 in
    List.iter (find_representatives new_classes) diff_list;
    let ranks, parents = List.fold_left (set_representatives new_classes) (b.ranks, parents) diff_list in
    { ranks; parents }

  (** {1 Debug operations}                                                    *)
  (****************************************************************************)

  let copy x = x

  let pretty fmt t =
    PersistentArray.pretty (fun fmt parent -> match parent with
      | Uninitialized -> Format.pp_print_string fmt "U"
      | Root _ -> Format.fprintf fmt "R"
      | Child c -> Format.fprintf fmt "C(%a)" Node.pretty c.parent
    ) fmt t.parents

  let rec count_rank t n i =
    match PersistentArray.get t i with
    | Root _ | Uninitialized -> (n,i)
    | Child c -> count_rank t (n+1) (Node.to_int c.parent)

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
