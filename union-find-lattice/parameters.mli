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

(** Common module types for the various functors creating union-finds lattices. *)

(** {1 Nodes}                                                                                     *)
(**************************************************************************************************)
(** Nodes are the elements on which a union-find operates. *)

(** Nodes stored in the union find. Nodes must inject into integers via {!NODE.to_int}. *)
module type NODE = sig
  type t
  (** The type of nodes *)

  val equal : t -> t -> bool
  (** Node equality *)

  val to_int : t -> int
  (** Returns a unique integer identifier associated with the given element (eg
      hash-consed tag). See {!PatriciaTree.HETEROGENEOUS_KEY.to_int} *)

  val pretty : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** Some implementations (namely MemCAD's) require {!NODE}s to be comparable and
    biject into integers (via {!NODE.to_int} and {!NODE_WITH_COMPARE.of_int}). *)
module type NODE_WITH_COMPARE = sig
  include NODE
  val compare: t -> t -> int
  val of_int: int -> t (** needed for MemCAD init *)
end

(** A polymorphic version of {!NODE}, the type {!NODE.t} now has a parameter
    {{!POLYMORPHIC_NODE.t}['a POLYMORPHIC_NODE.t]}. This is useful for
    polymorphic labeled union-find variants. *)
module type POLYMORPHIC_NODE = sig
  type 'a t
  (** The type of elements (nodes) in the union-find structure *)

  val polyeq : 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp
  (** polymorphic equality on elements, returns a witness of type equality when
      they are equal. *)

  val to_int : 'a t -> int
  (** Returns a unique integer identifier associated with the given element (eg
      hash-consed tag). See {!PatriciaTree.HETEROGENEOUS_KEY.to_int} *)

  val pretty : Format.formatter -> 'a t -> unit
  (** Pretty printer *)
end

(** {1 Relations}                                                                                 *)
(**************************************************************************************************)
(** Labeled union-find introduces labels, which we call relations here, between {!NODE}s.
    Classical union find has a single equivalence relation, which can be mimicked
    with a unit relation ([type t = unit] or [type ('a, 'b) t = Equiv : ('a,'a) t])
    Relations have a group structure. *)

(** A (possibly non-commutative) {{: https://en.wikipedia.org/wiki/Group_(mathematics)}group} structure, used to represent relations.

    {b Assumes} that any module [G] implementing this should verify the axioms of a monoid:
    - {b identity is neutral for compose}.
      For all [x], [G.compose x G.identity = G.compose G.identity x = x]
    - {b compose is associative}.
      For all [x] [y] [z], [G.compose x (G.compose y z) = G.compose (G.compose x y) z]
    - {b inversion}.
      For all [x], [G.compose x (G.inverse x) = G.compose (G.inverse x) x = G.identity]

    where [=] is [G.equal] *)
module type GROUP = sig
  type t
  (** The type of elements of the group. *)

  val equal : t -> t -> bool
  (** Equality of relations *)

  val pretty : Format.formatter -> t -> unit
  (** Pretty printer for relations *)

  val identity : t
  (** The identity relation *)

  val compose : t -> t -> t
  (** Monoid composition, written using the functional convention
      [compose f g] is {m f \circ g}.
      Should be associative, and compatible with {!identity}:
      - For all x, [G.compose x G.identity = G.compose G.identity x = x]
      - For all x y z, [G.compose x (G.compose y z) = G.compose (G.compose x y) z] *)

  val inverse : t -> t
  (** Group inversion, should verify for all x:
      [G.compose x (G.inverse x) = G.compose (G.inverse x) x = G.identity] *)

  val hash: t -> int
  (** a hashing function *)
end

(** Polymorphic version of {!GROUP}. Elements now have two parameters ['a] and ['b].
    This helps ensure composition is done in the right way. *)
module type POLYMORPHIC_GROUP = sig
  type ('a, 'b) t
  (** The type of elements of the group.
      Since these are used to represent relation between our generic union-find
      elements {!POLYMORPHIC_NODE.t}, they have two type parameters, so an
      [('a, 'b) t] represents a relation between ['a POLYMORPHIC_NODE.t] and
      ['b POLYMORPHIC_NODE.t] *)

  val equal : ('a, 'b) t -> ('a, 'b) t -> bool
  (** Equality of relations *)

  val pretty : Format.formatter -> ('a, 'b) t -> unit
  (** Pretty printer for relations *)

  val identity : ('a, 'a) t
  (** The identity relation *)

  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Monoid composition, written using the functional convention
      [compose f g] is {m f \circ g}.
      Should be associative, and compatible with {!identity}:
      - For all x, [G.compose x G.identity = G.compose G.identity x = x]
      - For all x y z, [G.compose x (G.compose y z) = G.compose (G.compose x y) z] *)

  val inverse : ('a, 'b) t -> ('b, 'a) t
  (** Group inversion, should verify for all x:
      [G.compose x (G.inverse x) = G.compose (G.inverse x) x = G.identity] *)

  val hash: ('a,'b) t -> int
end

(** {1 Values}                                                                                    *)
(**************************************************************************************************)
(** Values form a lattice that can be attached to a union-find, each equivalence
    class can then have an attached value. *)

(** The values associated with each equivalence class in the union-find.
    Values have a lattice structure.
    For flexibility, the lattice value operations also take the a {!NODE}
    (often the representative) as an extra argument. It can be ignored if
    irrelevant. *)
module type VALUE = sig
  type node
  (** the type of {!NODE}. *)

  type t
  (** The type of values. *)

  val equal : node -> t -> t -> bool
  (** Equality on values. *)

  val incl : node -> t -> t -> bool
  (** [incl x y] is true if [x] is included (smaller than) [y] (i.e. [x = meet x y] or [y = join x y]). *)

  val meet : node -> t -> t -> t
  (** Intersection of values *)

  val join : node -> t -> t -> t option
  (** Union of values, only required for the joins.
      Can return [None] for a top value that does not need to be stored. *)

  val pretty : node -> Format.formatter -> t -> unit
  (** Pretty-printer *)
end

(** For labeled union-find, values and relations ({!GROUP}) interact via a group action
    {{!RELATIONAL_VALUE.apply}[apply]}*)
module type RELATIONAL_VALUE = sig
  include VALUE

  type relation
  (** The type of relations, should match {!GROUP.t}. *)

  val apply : node -> t -> relation -> t
  (** [apply n v r] is the value obtained by applying relation [r] to value [v]
      at the node [n].
      [apply] should be a group action, meaning it should verify the following:
      - [apply _ v R.identity = v]
      - [apply _ (apply _ v r2) r1 = apply _ v (R.compose r2 r1)] *)
end

(** Polymorphic version of {!RELATIONAL_VALUE}. *)
module type POLYMORPHIC_VALUE = sig
  type 'a node
  (** The generic type of nodes, should match {!POLYMORPHIC_NODE.t}*)

  type 'a t
  (** The generic type of our values.
      An ['a t] value is associated to each class of our union find whose
      representative has type {{!POLYMORPHIC_NODE.t}['a POLYMORPHIC_NODE.t]}. *)

  val meet : 'a node -> 'a t -> 'a t -> 'a t
  (** Intersection of values *)

  val equal : 'a node -> 'a t -> 'a t -> bool
  (** Equality on values. *)

  val incl : 'a node ->'a t -> 'a t -> bool
  (** [incl x y] is true if [x] is included (smaller than) [y] (i.e. [x = meet x y] or [y = join x y]). *)

  val pretty : 'a node -> Format.formatter -> 'a t -> unit
  (** Pretty-printer *)

  val join : 'a node -> 'a t -> 'a t -> 'a t option
  (** Union of values, only required for [join].
      Can return [None] for a top value that does not need to be stored. *)

  type ('a, 'b) relation
  (** The type of relations, should match {!POLYMORPHIC_GROUP.t}. *)

  val apply : 'a node -> 'a t -> ('a, 'b) relation -> 'b t
  (** [apply v r] is the value obtained by applying relation [r] to value [v]
      [apply] should be a group action from
      {{!POLYMORPHIC_GROUP}[R : POLYMORPHIC_GROUP with type ('a,'b) t = ('a,'b) relation]}
      on the value ['a t]. Meaning it should verify the following:
      - [apply v R.identity = v]
      - [apply (apply v r2) r1 = apply v (R.compose r2 r1)] *)
end

(** {1 Configuration}                                                                             *)
(**************************************************************************************************)
(** The first functor argument is typically used to offer various choices for
    union-find implementation *)

(** Configuration options for standard array-with-copy union-finds *)
module type ARRAY_CONFIG = sig
  val path_compression: [`Lazy | `None]
  (** Should find perform path compression *)
end

(** Configuration options for patricia tree union-finds *)
module type PATRICIA_TREE_CONFIG = ARRAY_CONFIG

(** Configuration options for persistent array union-finds *)
module type PERSISTENT_ARRAY_CONFIG = sig
  include ARRAY_CONFIG (** @inline *)

  val extendable: bool
  (** Extendable arrays can be resized to access more nodes.
      Non-extendable arrays fail if called on a node whose {{!Parameters.NODE.to_int}[to_int]}
      is larger than the one passed to {{!Sig.UNION_FIND_LATTICE.make}[make]}, while
      extendable arrays will be resized. *)
end
