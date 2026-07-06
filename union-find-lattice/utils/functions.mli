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


val hash_pair : int -> int -> int
(** [hash_pair x y] returns a decent hash for the pair [(x,y)]. *)

val pp_option :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
(** [pp_option pp_some fmt opt] prints [pp_some fmt x] when [opt] is [Some x]
    and ["None"] otherwise. This is a far more reasonable default than
    [Format.pp_print_option]. *)

val range_fold: (int -> 'a -> 'a) -> int -> 'a -> 'a
(** [range_fold f n init] is [f 0 init |> f 1 |> ... |> f n] *)

val list_of_hashtbl_keys: 'a PersistentArray.IntHashtable.t -> int list
(** returns the list of keys present in the hashtable *)
