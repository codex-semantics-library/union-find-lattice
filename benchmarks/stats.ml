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

type exhaustive = |
type compact = |

type 'a number =
  | Int: int number
  | Float: float number

let init_number: type a. a number -> a = function
  | Int -> 0
  | Float -> 0.

(* A wrapper for the sum of squares, to avoid overflows *)
type 'a large_number =
  | WF : float -> float large_number
  | WI : Z.t -> int large_number

(** [add_square n x] is [n+x*x] *)
let add_square (type a) (n: a large_number) (x:a) : a large_number = match n with
  | WF sum -> WF (sum +. x*.x)
  | WI sum -> let x = Z.of_int x in WI Z.(sum + x * x)

let init_large_number: type a. a number -> a large_number = function
  | Int -> WI Z.zero
  | Float -> WF 0.

let large_number_to_float (WI z) = WF (Z.to_float z)
let float_of_large_number (type a) (x: a large_number) : float = match x with
  | WF f -> f
  | WI z -> Z.to_float z

let large_number_sum (type a) (x: a large_number) (y: a large_number) : a large_number = match x, y with
  | WF a, WF b -> WF (a +. b)
  | WI a, WI b -> WI Z.(a + b)

type 'a values =
  | Array of 'a array
  | List of 'a list
  | Cat of 'a values * 'a values
let rec values_map f = function
  | Array a -> Array (Array.map f a)
  | List l -> List (List.map f l)
  | Cat(l,r) -> Cat(values_map f l, values_map f r)

type ('a, _) t =
  | Compact : {
      typ: 'a number;
      size: int;
      sum: 'a;
      sum_squares: 'a large_number;
      min: 'a;
      max: 'a;
    } -> ('a, compact) t
  | Exhaustive : {
      compact: ('a, compact) t;
      mutable values: 'a values;
      mutable sorted: bool;
    } -> ('a, exhaustive) t

let size : type kind. ('a, kind) t -> int = function
  | Exhaustive { compact=Compact{ size; _ }; _; } -> size
  | Compact { size; _ } -> size

let typ (type kind) (x: ('a, kind) t) =  match x with
  | Exhaustive { compact=Compact{ typ; _; }; _; } -> typ
  | Compact { typ; _ } -> typ

let sort_values (type a) (Exhaustive x as ex: (a, _) t) : a array =
  if x.sorted then match x.values with
    | Array arr -> arr
    | _ -> assert false
  else
    let array = match x.values with
      | Array arr -> arr
      | _ -> let array = Array.make (size ex) (init_number (typ ex)) in
            let rec flatten_values i = function
              | Array arr -> Array.fold_left (fun i x -> array.(i) <- x; i+1) 0 arr
              | List l -> List.fold_left (fun i x -> array.(i) <- x; i+1) i l
              | Cat(l,r) -> flatten_values (flatten_values i l) r in
            let _ = flatten_values 0 x.values in array
    in
    Array.sort compare array;
    x.sorted <- true;
    x.values <- Array array;
    array

let compact_int_empty = Compact {
  typ=Int;
  size=0;
  min=0;
  max=0;
  sum=0;
  sum_squares=init_large_number Int;
}
let exhaustive_int_empty = Exhaustive {
  compact = compact_int_empty;
  values = Array [||];
  sorted = true;
}
let compact_float_empty = Compact {
  typ=Float;
  size=0;
  min=0.;
  max=0.;
  sum=0.;
  sum_squares=init_large_number Float;
}
let exhaustive_float_empty = Exhaustive {
  compact = compact_float_empty;
  values = Array [||];
  sorted = true;
}

let add_value_compact (type a) (Compact aggregate: (a, _) t) value : (a, _) t =
  let init = aggregate.size = 0 in
  let (+.): a -> a -> a = match aggregate.typ with
    | Int -> (+)
    | Float -> (+.)
  in Compact {
    typ = aggregate.typ;
    size = aggregate.size + 1;
    min = if init then value else min aggregate.min value;
    max = if init then value else max aggregate.max value;
    sum = aggregate.sum +. value;
    sum_squares = add_square aggregate.sum_squares value
  }

let add_value (type kind a) (aggregate: (a, kind) t) value : (a, kind) t =
  match aggregate with
  | Compact _ -> add_value_compact aggregate value
  | Exhaustive { values; compact; _ } -> Exhaustive {
      compact = add_value_compact compact value;
      values = (match values with
        | List l -> List (value::l)
        | _ -> Cat (values, List [value]));
      sorted = false
    }

let add_list_compact aggregate list = List.fold_left add_value_compact aggregate list
let add_array_compact aggregate array = Array.fold_left add_value_compact aggregate array

let compact_int_singleton value = add_value compact_int_empty value
let exhaustive_int_singleton i = Exhaustive {
  compact = compact_int_singleton i;
  values = Array [|i|];
  sorted = true;
}
let compact_float_singleton value = add_value compact_float_empty value
let exhaustive_float_singleton i = Exhaustive {
  compact = compact_float_singleton i;
  values = Array [|i|];
  sorted = true;
}

let add_list (type kind a) (aggregate: (a, kind) t) list: (a, kind) t =
  match aggregate with
  | Compact _ -> add_list_compact aggregate list
  | Exhaustive { values; compact; _ } -> Exhaustive {
      compact = add_list_compact compact list;
      values = Cat (values, List list);
      sorted = false;
    }
let add_array (type kind a) (aggregate: (a, kind) t) array: (a, kind) t =
  match aggregate with
  | Compact _ -> add_array_compact aggregate array
  | Exhaustive { values; compact; _ } -> Exhaustive {
      compact = add_array_compact compact array;
      values = Cat (values, Array (Array.copy array));
      sorted = false;
    }

let compact_of_int_list list = add_list_compact compact_int_empty list
let exhaustive_of_int_list list = Exhaustive {
  compact = compact_of_int_list list;
  values = List list;
  sorted = false;
}
let compact_of_float_list list = add_list_compact compact_float_empty list
let exhaustive_of_float_list list = Exhaustive {
  compact = compact_of_float_list list;
  values = List list;
  sorted = false;
}

let compact_of_int_array array = add_array_compact compact_int_empty array
let exhaustive_of_int_array array = Exhaustive {
  compact = compact_of_int_array array;
  values = Array array;
  sorted = false;
}
let compact_of_float_array array = add_array_compact compact_float_empty array
let exhaustive_of_float_array array = Exhaustive {
  compact = compact_of_float_array array;
  values = Array array;
  sorted = false;
}

let to_list x = sort_values x |> Array.to_list

let to_array x = sort_values x |> Array.copy

let make_compact (Exhaustive { compact; _ }) = compact
let compact (type kind) : ('a, kind) t -> ('a, compact) t = function
  | Compact _ as x -> x
  | Exhaustive { compact; _; } -> compact

let make_float_compact (Compact x) = Compact {
  typ = Float;
  size = x.size;
  min = float_of_int x.min;
  max = float_of_int x.max;
  sum = float_of_int x.sum;
  sum_squares = large_number_to_float x.sum_squares;
}

let make_float (type kind) : (int, kind) t -> (float, kind) t = function
  | Compact _ as x -> make_float_compact x
  | Exhaustive x -> Exhaustive {
      x with
      values = values_map float_of_int x.values;
      compact = make_float_compact x.compact;
    }

let concat_compact (type a) (Compact l as cl : (a, _) t) (Compact r as cr : (a, _) t) =
  if l.size = 0 then cr else
  if r.size = 0 then cl else
  let (+.): a -> a -> a = match l.typ with Int -> (+) | Float -> (+.) in
  Compact {
    typ = l.typ;
    size = l.size + r.size;
    min = min l.min r.min;
    max = max l.max r.max;
    sum = l.sum +. r.sum;
    sum_squares = large_number_sum l.sum_squares r.sum_squares;
  }

let concat : type kind a. (a, kind) t -> (a, kind) t -> (a, kind) t = fun cl cr ->
  match cl, cr with
  | Compact _, Compact _ -> concat_compact cl cr
  | Exhaustive { values=values_l; compact=cl; _ }, Exhaustive { values; compact; _ } ->
      Exhaustive {
        compact = concat_compact cl compact;
        values = Cat (values_l, values);
        sorted = false
      }

exception CollectionTooShort

let check_empty x = if size x = 0 then raise CollectionTooShort

let sum x =
  let Compact {sum;_;} as x = compact x in
  check_empty x; sum

let sum_squares x =
  let Compact {sum_squares;_;} as x = compact x in
  check_empty x; sum_squares

let min x =
  let Compact {min;_;} as x = compact x in
  check_empty x; min

let max x =
  let Compact {max;_;} as x = compact x in
  check_empty x; max


let to_float (type a kind) (x : (a, kind) t) : a -> float = match typ x with
  | Int -> float_of_int
  | Float -> Fun.id

let average x = (to_float x (sum x)) /. (float_of_int (size x))

(** Variance can be computed as {m E[X^2] - E[X]^2}, instead of the usual definition
    of {m E[(X - E(x))^2]}. *)
let variance x =
  let average = average x in
  let avg_square = (float_of_large_number (sum_squares x)) /. (float_of_int (size x))
  in avg_square -. average *. average

let sum_squares (type a) (x : (a, 'kind) t) : a =
  match sum_squares x with
  | WF f -> f
  | WI z -> Z.to_int z

let standard_deviation x = sqrt (variance x)

let standard_deviation_percent x = standard_deviation x *. 100. /. average x

let range (type a kind) (x: (a, kind) t) : a = match typ x with
  | Int -> max x - min x
  | Float -> max x -. min x

type pair_or_single = Single of int | Pair of int * int
let first = function Single x | Pair (x,_) -> x
let last = function Single x | Pair (_, x) -> x

let in_bounds array i = 0 <= i && i <= Array.length array

let get_median array to_float = function
  | Single i ->
    if in_bounds array i
    then to_float array.(i)
    else raise CollectionTooShort
  | Pair (i,j) ->
    if in_bounds array i && in_bounds array j
    then (to_float array.(i) +. to_float array.(j)) /. 2.
    else raise CollectionTooShort

(** Find the median position between [index_start] (included) and [index_end] excluded. *)
let median_indices index_start index_end =
  let len = index_end - index_start in
  let half = len / 2 in
  if len mod 2 == 0
  then Pair (index_start + half - 1, index_start + half)
  else Single (index_start + half)

let median x =
  check_empty x;
  let values = sort_values x in
  get_median values (to_float x) @@ median_indices 0 (size x)

let q1 x =
  if size x < 4 then raise CollectionTooShort;
  let values = sort_values x in
  get_median values (to_float x) @@ median_indices 0 (median_indices 0 (size x) |> first)
let q3 x =
  let size = size x in
  if size < 4 then raise CollectionTooShort;
  let values = sort_values x in
  get_median values (to_float x) @@ median_indices ((median_indices 0 size |> last) + 1) size

let stats_of_sum (type a) (s1: (a,_) t) s2 =
  let Compact s1 = compact s1 in
  let Compact s2 = compact s2 in
  assert (s1.size = s2.size);
  let (+.): a -> a -> a = match s1.typ with
    | Int -> (+)
    | Float -> (+.)
  in
  Compact {
    typ=s1.typ;
    size=s1.size;
    sum_squares=large_number_sum s1.sum_squares s2.sum_squares;
    sum=s1.sum +. s2.sum;
    min=s1.min +. s2.min;
    max=s1.max +. s2.max;
  }

(* Quick test*)
(* let () =
  let list = [0; 5; 7; 8; 19; 35; 64] in
  let stats = of_int_list list in
  let stats = add_list stats [89;-7] in
  assert (median stats = 8.);
  assert (q1 stats = 2.5);
  assert (q3 stats = 49.5);
  assert (range stats = 96);
  assert (sum stats = 220);
  assert (size stats = 9);
  assert (average stats = 24.4444444444444429);
  assert (variance stats = 934.69135802469134) *)

(** Division rounding instead of truncating,
    https://stackoverflow.com/a/18067292 *)
let round_division num denom =
  if (num < 0) = (denom < 0)
  then (num + denom/2) / denom
  else (num - denom/2) / denom

let pp_percent ?(justify=false) ?(precision=1) () fmt (num,denom) =
  let rec mutliplier acc = function 0 -> acc | n -> mutliplier (acc*10) (n-1) in
  let multiplier = mutliplier 1 precision in
  let per_multiplier = round_division (num * 100 * multiplier) denom in
  if precision = 0 then (* No "." in output *)
    Format.fprintf fmt (if justify then "%3d%%" else "%d%%") (per_multiplier / multiplier)
  else
    Format.fprintf fmt (if justify then "%3d.%d%%" else "%d.%d%%") (per_multiplier / multiplier) (per_multiplier mod multiplier)

let pp_with_unit unit_prefixes separator base fmt value =
  let len = List.length unit_prefixes - 1 in
  let rec scale value power =
    if value < base || power >= len
    then (value, power)
    else scale (value /. base) (power + 1)
  in
  let scaled, power = scale value 0 in
  begin
    if scaled >= 100. || power = 0 then Format.fprintf fmt "%.0f%s%s" scaled
    else if scaled >= 10. then Format.fprintf fmt "%.1f%s%s" scaled
    else Format.fprintf fmt "%.2f%s%s" scaled
  end separator (List.nth unit_prefixes power)

let unit_prefixes = [""; "k"; "M"; "G"; "T"; "P"; "E"; "Z"; "Y"; "R"; "Q" ]
let float_prefixes = ["m"; "µ"; "n"; "p"; "f"; "a"; "z"; "y"; "r"; "q"]

let pp_int_with_unit
    ?(justify=false)
    ?(unit_prefixes=unit_prefixes)
    ?(separator="")
    ?(base=1000) () fmt nb =
  let base = float_of_int base in
  let nb = float_of_int nb in
  if justify then
    let str = Format.asprintf "%a" (pp_with_unit unit_prefixes separator base) nb in
    let unit_length = List.fold_left (fun x elt -> Stdlib.max x (String.length elt)) 0 unit_prefixes in
    Format.fprintf fmt "%s" (String.make ((* 3 digits + fixed point + unit max length *)4+unit_length+(String.length separator) - String.length str) ' ' ^ str)
  else pp_with_unit unit_prefixes separator base fmt nb

let pp_float_with_unit
    ?justify
    ?(unit_prefixes=unit_prefixes) ?(float_prefixes=float_prefixes)
    ?separator
    ?(base=1000) () fmt nb =
  let rec find_integer float unit_prefixes float_prefixes =
    if float >= 100.0 then int_of_float float, unit_prefixes else
    match float_prefixes with
    | [] -> failwith "float too small"
    | u::us -> find_integer (float*. float_of_int base) (u::unit_prefixes) us
  in
  let int, unit_prefixes =
    if nb = 0.0
    then 0, unit_prefixes
    else find_integer nb unit_prefixes float_prefixes
  in pp_int_with_unit ?justify ~unit_prefixes ?separator ~base () fmt int
