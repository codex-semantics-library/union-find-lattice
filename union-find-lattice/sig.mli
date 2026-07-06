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

(** The common signatures of union-finds. Each implementation should have the same
    interface, among one of those below. *)

(** {1 Union-find lattice signatures}                                                             *)
(**************************************************************************************************)

(** The simplest union-find lattice. *)
module type UNION_FIND_LATTICE = sig
  type node
  (** the type of nodes, i.e. elements of the union-find. *)

  type t
  (** the type of the union-find *)

  (** {1 Union-find operations}                                               *)
  (****************************************************************************)

  val make: int -> t
  (** [make n] creates a new union-find supporting up to [n] {!node}s:
      - {{!Classic.PatriciaTree}[PatriciaTree]} implementations disregard this [n] and can
        always support any number of nodes.
      - {{!Classic.ArrayWithCopy}[ArrayWithCopy]} will crash if used with {!node} whose {!NODE.to_int} is [>= n].
      - {{!Classic.PersistentArray}[PersistentArray]} will use [n] to set the initial array size.
        If {{!Parameters.PERSISTENT_ARRAY_CONFIG.extendable}[PERSISTENT_ARRAY_CONFIG.extendable]} is [true]
        (the default), the array may then grow beyond that size if needed. Otherwise it will fail. *)

  val find: t -> node -> node
  (** [find uf n] returns a unique representative of the node [n] in [uf]. *)

  val union: t -> node -> node -> t
  (** [union uf a b] returns a new version of [uf] where the classes of [a] and
      [b] have been merged. *)

  val check_related: t -> node -> node -> bool
  (** [check_related uf a b] is [true] if and only if [a] and [b] are in the
      same equivalence class in [uf]. *)

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  val incl: t -> t -> bool
  (** [incl uf_a uf_b] is lattice inclusion, [uf_a <= uf_b], i.e.
      forall [x] [y], if [check_related uf_b x y] then [check_related uf_a x y] *)

  val join: t -> t -> t
  (** [join uf_a uf_b] is the least upper-bound of [uf_a] and [uf_b], i.e.
      forall [x] [y], [check_related (join uf_a uf_b) x y] iff
        [check_related uf_b x y] and [check_related uf_a x y] *)

  val meet: t -> t -> t
  (** [meet uf_a uf_b] is the greatest lower bound of [uf_a] and [uf_b],
      its relation [check_related (meet uf_a uf_b)] is the transitive closure
      of [check_related uf_a] or [check_related uf_b]. *)

  (** {1 Testing and debugging}                                               *)
  (****************************************************************************)

  val copy: t -> t
  (** To compare persistent union-find with non-persistent version, we need an
      explicit copy operation. This is [Fun.id] for all persistent versions. *)

  val check_invariants: t -> string option
  (** For testing, [check_invariant uf] returns [None] if all internal invariants hold.
      Otherwise, it returns [Some error_msg]. *)

  val pretty: Format.formatter -> t -> unit
  (** pretty printer. *)
end

(** Extend {!UNION_FIND_LATTICE} with values attached to each equivalence class. *)
module type VALUED_UNION_FIND_LATTICE = sig
  include UNION_FIND_LATTICE (** @open *)

  (** {1 Values}                                                              *)
  (****************************************************************************)

  type value
  (** the type of values, one optional value is attached to each equivalence class. *)

  val get_value: t -> node -> value option
  (** [get_value uf n] returns the optional value attached to the class of [n]. *)

  val set_value: intersect:bool -> t -> node -> value -> t
  (** [set_value ~intersect uf n v] returns a new union-find, where the value
      attached to the class of [n] is now [v]. If [intersect] is true, this value
      will be combined with the previous value (should one exists) via {!Parameters.VALUE.meet}. *)
end


(** Signature for a labeled union-find lattice. Its parent edges are annotated
    by {{!LABELED_LABELED_UNION_FIND_LATTICE.relation}[relation]}s, which form a
    {{!Parameters.GROUP}[Group]}. *)
module type LABELED_UNION_FIND_LATTICE = sig
  type node
  (** The type of union-find nodes i.e. the elements of the partition set *)

  type relation
  (** The type of relations between {!nodes}. These relations have a
      {{: https://en.wikipedia.org/wiki/Group_(mathematics)}group structure}. *)

  type t
  (** The type of the persistent labeled union-find (PUF) data-structure *)

  (** {1 Union-find operations}                                               *)
  (****************************************************************************)

  val make: int -> t
  (** Create a new union-find, see {!UNION_FIND_LATTICE.make} *)

  val find: t -> node -> node * relation
  (** [find uf x] returns:
      - the reprensentative of [x]
      - the relation between [x] and its representative
      It performs both the 'find' and 'get_value' operations from the paper. *)

  val add_relation: t -> node -> node -> relation -> (t, relation) result
  (** [add_relation uf x y r] returns a new persistent union-find, where the
      relation [x --(r)--> y] was added.

      This is the {{!UNION_FIND_LATTICE.union}[union]} operation of classical union find, only this time it takes
      the new relation as an extra argument.

      This fails if [x] and [y] were already related by a different relation in [uf].
      In that case, that other relation is returned. *)

  val check_related: t -> node -> node -> relation option
  (** [check_related uf x y] checks if [x] and [y] are in the same relational class
      in [uf]. If so, the relation between them is returned. Otherwise, [None]
      is returned. *)

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  val incl: t -> t -> bool
  (** [incl uf_a uf_b] is lattice inclusion, [uf_a <= uf_b], i.e.
      forall [x] [y], if [check_related uf_b x y] then [check_related uf_a x y] *)

  val join: t -> t -> t
  (** [join uf_a uf_b] is the least upper-bound of [uf_a] and [uf_b], i.e.
      forall [x] [y], [check_related (join uf_a uf_b) x y] iff
        [check_related uf_b x y] and [check_related uf_a x y] *)

  val meet: t -> t -> t * (node * relation * node) list
  (** [meet uf_a uf_b] is a pair [meet, extra_rel]:
      - [meet] is (one of) the greatest lower bound of [uf_a] and [uf_b].
        It contains all relations of [uf_a] and some of the relations of [uf_b].
        (All nodes that are related in [uf_b] are still related, but their relation
        might be changed, as all relations may not be preserved)
      - [extra_rel] is a minimal list of relations from [uf_b] that could not be
        preserved. For example, if [uf_a] contains [x --(r)--> y] and [uf_b]
        contains [x --(r')--> y] with [r != r'], [r'] will either be in the list,
        or a transitive/symmetric consequence of relations that are in the list. *)

  (** {1 Testing and debugging}                                               *)
  (****************************************************************************)

  val copy: t -> t
  (** To compare persistent union-find with non-persistent version, we need an
      explicit copy operation. This is [Fun.id] for all persistent versions. *)

  val check_invariants: t -> string option
  (** [check_invariants uf] is used in testing, to assert that the data-structures
      internal invariants, if any, are still true after complex operations.

      Returns [None] if they hold, [Some error_msg] otherwise. *)

  val pretty: Format.formatter -> t -> unit
  (** pretty-printer *)
end

(** Extension of {!LABELED_UNION_FIND_LATTICE} which
    can store an optional {{!LABELED_VALUED_UNION_FIND_LATTICE.value}[value]}
    on each relational class. *)
module type LABELED_VALUED_UNION_FIND_LATTICE = sig
  include LABELED_UNION_FIND_LATTICE

  type value
  (** The type of values assigned to relational classes. Relation act as
      {{: https://en.wikipedia.org/wiki/Group_action}group actions} on values:
      given a {{!relation}[('a, 'b) relation]}, we can transform a {{!value}['a value]}
      into a {{!value}['b value]} *)

  val get_value: t -> node -> value option
  (** [get_value uf x] returns the value attached to [x], if it exists. *)

  val set_value: intersect:bool -> t -> node -> value -> t
  (** [set_value ~intersect uf n v] returns a new union-find, where the value
      attached to the class of [n] is now [v]. If [intersect] is true, this value
      will be combined with the previous value (should one exists) via {!Parameters.VALUE.meet}. *)
end

(** Polymorphic version of {!LABELED_UNION_FIND_LATTICE}, here the type of nodes has
    a parameter: {{!POLYMORPHIC_LABELED_UNION_FIND_LATTICE.node}['a node]}
    and the type of relations has two: {{!POLYMORPHIC_LABELED_UNION_FIND_LATTICE.relation}[('a,'b) relation]}.
    This allows type-checking of relation composition/inversion, at the cost
    of using GADTs and existential types. *)
module type POLYMORPHIC_LABELED_UNION_FIND_LATTICE = sig
 type 'a node
  (** The type of union-find nodes i.e. the elements of the partition set *)

  type ('a, 'b) relation
  (** The type of relations. A {{!relation}[('a, 'b) relation]} relates an {{!node}['a node]} and a
      {{!node}['b node]}. Mathematically, relations have a
      {{: https://en.wikipedia.org/wiki/Group_(mathematics)}group structure}. *)

  type t
  (** The type of the persistent union-find (PUF) data-structure *)

  (** The true type of {!find} is
      ['a t -> 'a node -> ∃ 'b. 'b t * ('a, 'b) relation * 'b value option]
      Since we can't express existential types directly in OCaml, we need this wrapper type. *)
  type 'a find_result = FindResult: {
    representative: 'b node;
    relation: ('a, 'b) relation;
  } -> 'a find_result

  val make: int -> t
  (** Create a new union-find, see {!UNION_FIND_LATTICE.make} *)

  val find: t -> 'a node -> 'a find_result
  (** [find uf x] returns:
      - the reprensentative of [x]
      - the relation between [x] and its representative
      It performs both the 'find' and 'get_value' operations from the paper. *)

  val add_relation: t -> 'a node -> 'b node -> ('a, 'b) relation -> (t, ('a,'b) relation) result
  (** [add_relation uf x y r] returns a new persistent union-find, where the
      relation [x --(r)--> y] was added.

      This is the {{!UNION_FIND_LATTICE.union}[union]} operation of classical union find, only this time it takes
      the new relation as an extra argument.

      This fails if [x] and [y] were already related by a different relation in [uf].
      In that case, that other relation is returned. *)

  val check_related: t -> 'a node -> 'b node -> ('a, 'b) relation option
  (** [check_related uf x y] checks if [x] and [y] are in the same relational class
      in [uf]. If so, the relation between them is returned. Otherwise, [None]
      is returned. *)

  (** {1 Lattice operations}                                                  *)
  (****************************************************************************)

  val incl: t -> t -> bool
  (** [incl uf_a uf_b] is lattice inclusion, [uf_a <= uf_b], i.e.
      forall [x] [y], if [check_related uf_b x y] then [check_related uf_a x y] *)

  val join: t -> t -> t
  (** [join uf_a uf_b] is the least upper-bound of [uf_a] and [uf_b], i.e.
      forall [x] [y], [check_related (join uf_a uf_b) x y] iff
        [check_related uf_b x y] and [check_related uf_a x y] *)

  (** Existential wrapper for a relation between two nodes, used by {!meet}. *)
  type wrapped_relation =
    Rel: 'a node * ('a, 'b) relation * 'b node -> wrapped_relation

  val meet: t -> t -> t * wrapped_relation list
  (** [meet uf_a uf_b] is a pair [meet, extra_rel]:
      - [meet] is (one of) the greatest lower bound of [uf_a] and [uf_b].
        It contains all relations of [uf_a] and some of the relations of [uf_b].
        (All nodes that are related in [uf_b] are still related, but their relation
        might be changed, as all relations may not be preserved)
      - [extra_rel] is a minimal list of relations from [uf_b] that could not be
        preserved. For example, if [uf_a] contains [x --(r)--> y] and [uf_b]
        contains [x --(r')--> y] with [r != r'], [r'] will either be in the list,
        or a transitive/symmetric consequence of relations that are in the list *)



  (** {1 Testing and debugging}                                               *)
  (****************************************************************************)

  val copy: t -> t
  (** To compare persistent union-find with non-persistent version, we need an
      explicit copy operation. This is [Fun.id] for all persistent versions. *)

  val check_invariants: t -> string option
  (** [check_invariants uf] is used in testing, to assert that the data-structures
      internal invariants, if any, are still true after complex operations.

      Returns [None] if they hold, [Some error_msg] otherwise. *)

  val pretty: Format.formatter -> t -> unit
  (** pretty-printer *)
end

(** Extension of {!POLYMORPHIC_LABELED_UNION_FIND_LATTICE} which
    can store an optional {{!POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE.value}['a value]}
    on each relational class. *)
module type POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE = sig
  include POLYMORPHIC_LABELED_UNION_FIND_LATTICE

  type 'a value
  (** The type of values assigned to relational classes. Relation act as
      {{: https://en.wikipedia.org/wiki/Group_action}group actions} on values:
      given a {{!relation}[('a, 'b) relation]}, we can transform a {{!value}['a value]}
      into a {{!value}['b value]} *)

  val get_value: t -> 'a node -> 'a value option
  (** [get_value uf x] returns the value attached to [x], if it exists. *)

  val set_value: intersect:bool -> t -> 'a node -> 'a value -> t
  (** [set_value ~intersect uf n v] returns a new union-find, where the value
      attached to the class of [n] is now [v]. If [intersect] is true, this value
      will be combined with the previous value (should one exists) via {!Parameters.VALUE.meet}. *)
end
