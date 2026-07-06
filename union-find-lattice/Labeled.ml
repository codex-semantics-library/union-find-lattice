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

module Monomorphic_of_polymorphic
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP)
  (UF: functor
    (Node: Parameters.POLYMORPHIC_NODE)
    (Relation: Parameters.POLYMORPHIC_GROUP)
   -> Sig.POLYMORPHIC_LABELED_UNION_FIND_LATTICE
    with type 'a node = 'a Node.t
     and type ('a,'b) relation = ('a,'b) Relation.t
  ) = struct
  include UF(struct
    include Node
    type _ t = Node.t
    let polyeq: type a b. a t -> b t -> (a,b) PatriciaTree.cmp = fun (x: a t) (y: b t) ->
      if Node.equal x y then Obj.magic PatriciaTree.Eq else PatriciaTree.Diff
  end)(struct
    include Relation
    type (_,_) t = Relation.t
  end)

  type node = Node.t
  type relation = Relation.t

  let find t x =
    let FindResult{representative;relation} = find t x in
    representative, relation

  let meet u v =
    let r, errs = meet u v in
    r, List.map (fun (Rel(x,r,y)) -> (x,r,y)) errs
end

module ArrayWithCopy
  (Config: Parameters.ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP)
= Monomorphic_of_polymorphic(Node)(Relation)(Polymorphic.ArrayWithCopy(Config))

module PatriciaTree
  (Config: Parameters.PATRICIA_TREE_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP)
= Monomorphic_of_polymorphic(Node)(Relation)(Polymorphic.PatriciaTree(Config))

module PersistentArray
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP)
= Monomorphic_of_polymorphic(Node)(Relation)(Polymorphic.PersistentArray(Config))

module PersistentArrayNCA
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP)
= Monomorphic_of_polymorphic(Node)(Relation)(Polymorphic.PersistentArrayNCA(Config))
