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

let eval_binop = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Div -> ( / )
  | Prod -> ( * )
  | Modulo -> ( mod )

let rec eval_aexpr eval_var = function
  | Const c -> c
  | Var v -> eval_var v
  | Binop(op,l,r) -> eval_binop op (eval_aexpr eval_var l) (eval_aexpr eval_var r)

let eval_binpred = function
  | Eq -> ( = )
  | Lt -> ( < )
  | Le -> ( <= )

let rec eval_bexpr eval_var = function
  | Binpred(bin, l, r) -> eval_binpred bin (eval_aexpr eval_var l) (eval_aexpr eval_var r)
  | Or(l,r) -> eval_bexpr eval_var l || eval_bexpr eval_var r
  | And(l,r) -> eval_bexpr eval_var l && eval_bexpr eval_var r
  | True -> true
  | False -> false

let pp_binop fmt = function
  | Plus -> Format.fprintf fmt "+"
  | Minus -> Format.fprintf fmt "-"
  | Div -> Format.fprintf fmt "/"
  | Prod -> Format.fprintf fmt "*"
  | Modulo -> Format.fprintf fmt "mod"

let pp_binpred fmt = function
  | Eq -> Format.fprintf fmt "="
  | Lt -> Format.fprintf fmt "<"
  | Le -> Format.fprintf fmt "\\\\leq"

let rec pp_aexp pp_var fmt = function
  | Const c -> Format.pp_print_int fmt c
  | Var v -> Format.pp_print_string fmt (pp_var v)
  | Binop(o,l,r) -> Format.fprintf fmt "%a %a %a" (pp_aexp pp_var) l pp_binop o (pp_aexp pp_var) r

let rec pp_bexpr pp_var fmt = function
  | Binpred(o,l,r) -> Format.fprintf fmt "%a %a %a" (pp_aexp pp_var) l pp_binpred o (pp_aexp pp_var) r
  | And(l,r) -> Format.fprintf fmt "%a; %a" (pp_bexpr pp_var) l (pp_bexpr pp_var) r
  | Or(l,r) -> Format.fprintf fmt "%a || %a" (pp_bexpr pp_var) l (pp_bexpr pp_var) r
  | True -> Format.fprintf fmt "True"
  | False -> Format.fprintf fmt "False"
