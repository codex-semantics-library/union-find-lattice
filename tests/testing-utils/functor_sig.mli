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

open Union_Find_Lattice

(** {1 Union-find functors}                                                                       *)
(**************************************************************************************************)
(** Standard signature of functors, taking {!Parameters} as arguments and return a lattice with
    matching types. *)

module type UNION_FIND_LATTICE_FUNCTOR =
  functor (N: Parameters.NODE_WITH_COMPARE)
  -> Sig.UNION_FIND_LATTICE with type node = N.t

module type VALUED_UNION_FIND_LATTICE_FUNCTOR =
  functor
    (N: Parameters.NODE_WITH_COMPARE)
    (V: Parameters.VALUE with type node = N.t)
  -> Sig.VALUED_UNION_FIND_LATTICE with type node = N.t and type value = V.t

module type NODAL_VALUED_UNION_FIND_LATTICE_FUNCTOR =
  functor
    (N: Parameters.NODE)
    (V: Parameters.VALUE with type node = N.t)
  -> Sig.VALUED_UNION_FIND_LATTICE with type node = N.t and type value = V.t

module type LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR =
  functor
    (Node : Parameters.NODE)
    (Relation : Parameters.GROUP)
    (Value : Parameters.RELATIONAL_VALUE with type relation = Relation.t and type node = Node.t)
  -> Sig.LABELED_VALUED_UNION_FIND_LATTICE
    with type node = Node.t
     and type value = Value.t
     and type relation = Relation.t

module type POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR =
  functor
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP)
    (Value : Parameters.POLYMORPHIC_VALUE with type ('a, 'b) relation = ('a, 'b) Relation.t and type 'a node = 'a Node.t)
  -> Sig.POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE
    with type 'a node = 'a Node.t
     and type 'a value = 'a Value.t
     and type ('a, 'b) relation = ('a, 'b) Relation.t
