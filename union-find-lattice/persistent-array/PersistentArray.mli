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

module IntHashtable: Hashtbl.S with type key = int

(** Persistent array implementation. I.E. array which keep track of their history
    and allow rollback. They appear immutable. This implementation is pretty much
    exactly the one described by
    {{:https://dl.acm.org/doi/10.1145/1292535.1292541}\[Cochon and Filliâtre, 2007\]}. *)
module type S = sig
  (** Every array operation {!get}, {!set}, {!size}, {!diff} reroot's the array at
      the given version. For best performance, avoid jumping between versions too
      often. *)

  type 'a t
  (** The type of persistent arrays *)

  (** {1 Array creation}                                                        *)
  (******************************************************************************)

  val init: int -> (int -> 'a) -> 'a t
  (** [init n f] creates the array [[|f 0; f 1; ...; f (n-1)|]].
      [O(n)] complexity. *)

  val make: int -> 'a -> 'a t
  (** [make n x] creates the array [[|x; x; ...; x|]] with length [n].
      [O(n)] complexity.  *)

  val of_array: 'a array -> 'a t
  (** [of_array arr] creates the persistent array containing the same elements as [arr]
      This copies the array, so future modifications to [arr] will not corrupt the persistent array.
      [O(Array.length arr)] complexity.  *)

  val of_list: 'a list -> 'a t
  (** [of_list l] creates a persistent array with the same elements as [l].
      [O(List.length l)] complexity. *)


  (** {1 Array operations}                                                      *)
  (******************************************************************************)

  (** All operations {b reroot} the given array. This is constant time for mutliple
      accesses to the same version. When switching versions however, the cost is
      linear in the number of updates ({!set} operations) between both version. *)

  val size: 'a t -> int
  (** [size t] is the size (number of elements) of the array [t]*)

  val get: 'a t -> int -> 'a
  (** [get t i] returns the [i]-th element of the array.
      [O(1)] complexity (after reroot).
      @raises Invalid_argument if [i] is not in [0 .. (size t - 1)] *)

  val set: 'a t -> int -> 'a -> 'a t
  (** [set t i v] returns a new array whose values are the same as [t] except at
      position [i], where the value is [v].
      [O(1)] complexity (after reroot).
      @raises Invalid_argument if [i] is not in [0 .. (size t - 1)] *)

  val pretty:
      ?pp_sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a t -> unit
  (** [pretty ~pp_sep pp_elt t] prints the array at [t], using [pp_elt] to print
      elements and [pp_sep] to separate them. [pp_sep] defaults to printing a semicolon and a space). *)

  val diff: 'a t -> 'a t -> ('a * 'a) IntHashtable.t * 'a t option
  (** [diff a b] create a table of differences [i -> (a.(i), b.(i))] between [a] and [b]
      Requires [a] and [b] to share a common root (i.e. derive from the same {!init} or {!make}).

      The diff table may include identical values [i -> (v,v)], these correspond
      to changes that are reverted on the chain from [a] to [b]. For example:
      [let b = set (set a i x) i a.(i)].

      For performance: have [a] be the one closest to reroot (i.e. the last modified value).

      [O(d)] complexity (after reroot at [a]), where [d] is the number of updates
      ({!set} operations) between [a] and [b].

      {!Versioned} persistent arrays also return the element with the lowest
      version tag. *)

  val diff_key: 'a t -> 'a t -> unit IntHashtable.t * 'a t option
  (** [diff_key a b] is the same as {{!diff}[diff a b]}, but only returns the keys
      [i] such that [a.(i)] and [b.(i)] may be different.

      {!Versioned} persistent arrays also return the element with the lowest
      version tag. *)

  (** {1 Array resizing}                                                        *)
  (******************************************************************************)

  (** It is possible to extend persistent arrays (i.e. add elements at the back).
      These operations retro-actively modify all previous versions. They will all
      appear as though they always had the new size, so a {{!get}[get old_version i]}
      that would have failed before resize will now succeed. *)

  val append: 'a t -> 'a array -> unit
  (** [append t arr] extends [t] by adding the values from [arr] at the end. This
      modifies [t] and all previous versions of [t].
      [O(size t + Array.length arr)] complexity (after reroot).
      @raises Invalid_argument if [size t + Array.length arr > Sys.max_array_length] *)

  val extend: 'a t -> int -> 'a -> unit
  (** [extend t n x] extends [t] by appending [n] times the value [x]:
      [[|t.(0); ...; t.(size t - 1); x; ...; x|]]. This modifies [t] and all
      previous versions of [t].
      [O(size t + n)] complexity.
      @raises Invalid_argument if [n] is negative or if [size t + n > Sys.max_array_length]. *)

  (** {1 Iterators}                                                             *)
  (******************************************************************************)

  val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] creates a new persistent array, initialized by
      [[|f (get t 0); ...; f (get t (size t) - 1)|]] *)

  val iter: ('a -> unit) -> 'a t -> unit
  (** [iter f t] calls [f] on every element in order:
      [f (get t 0); ...; f (get t (size t - 1))] *)

  val fold: ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  (** [fold f init t] is [f (... f init (get t 0) ...) (get t (size t - 1))] *)
end

include S

(** {1 Versioned persistent array}                                            *)
(******************************************************************************)

(** Extension of persistent arrays with a version number. Allows {!diff}
    and {!diff_key} to also return the minimum version. *)
module Versioned : S
