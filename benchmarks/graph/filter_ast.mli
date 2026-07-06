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

type binop = Plus | Minus | Div | Prod | Modulo

type aexpr =
  | Const of int
  | Var of string
  | Binop of binop * aexpr * aexpr

type binpred = Eq | Lt | Le

type bexpr =
  | Binpred of binpred *aexpr * aexpr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | True | False

val eval_bexpr: (string -> int) -> bexpr -> bool
(** [eval_bexpr eval_var b] evaluates the boolean expression, using [eval_var]
    for variables *)

val pp_bexpr: (string -> string) -> Format.formatter -> bexpr -> unit
(** [pp_bexpr pp_var] prints the boolean expression. *)
