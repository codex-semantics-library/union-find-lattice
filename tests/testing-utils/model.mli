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

module Node : sig
    type _ t = IntTerm : int -> int t [@@unboxed]

    include Union_Find_Lattice.Parameters.POLYMORPHIC_NODE with type 'a t := 'a t
  end

module INode : Union_Find_Lattice.Parameters.NODE_WITH_COMPARE with type t = int Node.t


module Relation :
  sig
    type ('a, 'b) t =
      | Equal : ('a, 'a) t
      | Add : Z.t -> (int, int) t

    include Union_Find_Lattice.Parameters.POLYMORPHIC_GROUP with type ('a, 'b) t := ('a, 'b) t

    val of_int: int -> (int, int) t
    val debug_print: Format.formatter -> ('a,'b) t -> unit
  end

module Value :
  sig
    type _ t =
      | Empty : int t
      | Interval : Z.t option * Z.t option -> int t

    include Union_Find_Lattice.Parameters.POLYMORPHIC_VALUE
      with type ('a, 'b) relation = ('a, 'b) Relation.t
       and type 'a t := 'a t
       and type 'a node = 'a Node.t

    val interval : Z.t option -> Z.t option -> int t
    val debug_print: Format.formatter -> 'a t -> unit
  end

module IValue : Union_Find_Lattice.Parameters.VALUE with type t = int Value.t and type node = INode.t

(** Reference implementation of persistent union-find: a classical, array-based
    union-find with an explicit {!copy} function. *)

type t

val make : int -> t
val find : t -> int Node.t -> int Node.t * (int, int) Relation.t * int Value.t option
val check_related : t -> int Node.t -> int Node.t -> (int, int) Relation.t option
val get_value : t -> int Node.t -> int Value.t option
val add_value : ?no_apply:bool -> t -> int Node.t -> int Value.t -> unit
val add_relation : t -> int Node.t -> int Node.t -> (int, int) Relation.t -> (unit, (int, int) Relation.t) result
val join : t -> t -> t
val copy : t -> t
