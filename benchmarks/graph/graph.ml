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

type output = LaTeX of string | PNG of string | SVG of string
let output = PNG "graph"

type graph_style = Line | LinePoints | Errorbars of int | Errorshade of int (* nb of std_dev to show *)
let graph_style = LinePoints
type shade_style = Pattern | Color
let shade_style = Color

let results_folder = "graphs"


type data = {
  time: float;
  time_std_dev: float;
  mem: float;
  mem_std_dev: float;
}

module SMap = Map.Make(String)
type row = {
  name: string;
  params: int SMap.t;
  data: data SMap.t;
}

let rec range_fold f lo hi acc =
  if lo = hi then acc else range_fold f (lo + 1) hi (f lo acc)


let array_foldi f arr init =
  Array.fold_left (fun (acc, i) elt -> f i elt acc, i + 1) (init, 0) arr |> fst

let read_csv file =
  let csv = Csv.load file |> Csv.to_array in
  let categories = array_foldi (fun i elt categories ->
    if elt = "Parameters" || elt = ""
    then categories
    else (elt, i)::categories) csv.(0) [] |> List.rev in
  (* List.iter (fun (name, pos) -> Format.printf "%d: %s@." pos name) categories *)
  let (_, param_end) = List.hd categories in
  array_foldi (fun i row rows ->
    if i < 2 (* headers *) then rows else
      let name = row.(0) in
      let params = range_fold (fun j ->
        try let nb = int_of_string row.(j) in
        SMap.add csv.(1).(j) nb
      with Failure _ -> Format.printf "Non-int: %d %d %s %s@." i j csv.(1).(j) row.(j); failwith ""
        ) 1 param_end SMap.empty in
      let data = List.fold_left (fun data (name,i) ->
        SMap.add name {
          time=float_of_string row.(i);
          time_std_dev=float_of_string row.(i+1);
          mem=float_of_string row.(i+2);
          mem_std_dev=float_of_string row.(i+3)} data
        ) SMap.empty categories in
      {name;params;data}::rows
   ) csv []

let smap_add_elt k v smap = match SMap.find_opt k smap with
| None -> SMap.add k [v] smap
| Some l -> SMap.add k (v::l) smap

type plot_y_axis = Time | Mem
let label_y = function
  | Time -> "Time (s)"
  | Mem -> "Alloc (b)"

let get_data y_axis p series data =
  let p = SMap.find p data.params |> float_of_int in
  SMap.fold (fun k x acc ->
    let info = match y_axis with
      | Time -> (p, x.time, x.time_std_dev *. x.time /. 100.)
      | Mem -> (p, x.mem, x.mem_std_dev *. x.mem /. 100.) in
    smap_add_elt k info acc) data.data series

let format_title title =
  title
  |> Str.global_replace (Str.regexp_string "_") " "
  |> Str.global_replace (Str.regexp_string " && ") "; "

let all = ["PT_PC_UBR";"PA_PC_UBR";"PA_PC_UBR_Ver";] |> List.rev (* "SPA_PC_UBR";"A_PC_UBR"] *)

let caption = function
  | "PT_PC_UBR" -> "Patricia tree", Some(`Rgb(255,0,0)), Some 1, Some (`TranspPattern 1) (* red *)
  | "PA_PC_UBR" -> "Persistent array", Some(`Rgb(0,0,255)), Some 2, Some (`TranspPattern 4) (* blue *)
  | "PAS_PC_UBR" -> "Sparse persistent array", Some(`Rgb(0,255,0)), Some 3, Some (`TranspPattern 2) (* green *)
  | "PAN_PC_UBR" -> "Persistent array NCA", Some(`Rgb(116,40,129)), Some 4, Some(`TranspPattern 5)  (* purple *)
  | "A_PC_UBR_LJ" -> "Linear (Array w/ copy)", Some(`Rgb(253,179,056)), Some 5, Some(`TranspPattern 6) (* yellow *)
  | "FMC_EPC_SU_OJ" -> "Class intersection", None, None, None
  | "HPT_PC_UBR" -> "Hash-consed Patricia tree", None, None, None
  | "PT_PC_RU" -> "Patricia tree (random union)", None, None, None
  | "PA_PC_RU" -> "Persistent array (random union)", None, None, None
  | s -> format_title s, None, None, None

let plot_data graph_style is_latex title dash_type data =
  let data = List.sort_uniq (fun (l,_,_) (r,_,_) -> Float.compare l r) data in
  let title, color, point_type, fill = caption title in
  let fill = match shade_style with Pattern -> fill | Color -> Some `Solid in
  let title = if is_latex then None else Some title in
  match graph_style with
  | Errorbars n ->
        [ Gnuplot.Series.yerror_bars ?title ?color ?point_type ?dash_type
        (List.map (fun (x,y,z) -> (x,y,z*.float_of_int n)) data) ]
  | Errorshade n ->
        Gnuplot.Series.yerror_shade ?title ?color ?fill ?point_type ?dash_type
        (List.map (fun (x,y,z) -> (x,y,z*.float_of_int n)) data)
  | Line ->
        [ Gnuplot.Series.lines_xy ?title ?color ?dash_type
        (List.map (fun (x,y,_) -> (x,y)) data) ]
  | LinePoints ->
        [ Gnuplot.Series.linespoints_xy ?title ?color ?dash_type
        (List.map (fun (x,y,_) -> (x,y)) data) ]

let latex_var = function
  | "n" -> "$\\\\cardinal \\\\UFNode$"
  | "u" -> "$\\\\EvalU$"
  | "d" -> "$\\\\EvalD$"
  | c -> c

let make_title ~y_axis ~x_axis bench filter is_latex graph_style =
  if is_latex then None else Option.some @@
  let fmt = if is_latex then latex_var else Fun.id in
  let title =
  match y_axis with
  | Time ->
      Format.asprintf "%s runtime as a function of %s (%a)"
      (format_title bench) (fmt x_axis) (Filter_ast.pp_bexpr fmt) filter
  | Mem ->
      Format.asprintf "%s allocs as a function of %s (%a)"
      (format_title bench) (fmt x_axis) (Filter_ast.pp_bexpr fmt) filter
  in match graph_style, is_latex with
  | Errorbars n, false -> title ^ " (errorbars of "^string_of_int n^" standard deviation)"
  | _ -> title

let matches_single_select select title =
  String.split_on_char '_' title |>
  List.exists (fun s -> s = select)

let matches_select select title =
  select = "" ||
  String.split_on_char '&' select
  |> List.for_all (fun s ->
    let s = String.trim s in
    s = "" || if s.[0] = '!'
              then not (matches_single_select (String.sub s 1 (String.length s - 1)) title)
              else matches_single_select s title)


(** Merge list while somewhat preserving draw order *)
let rec my_flat_list first last = function
  | [] -> first @ last
  | [t;q]::r -> my_flat_list (t::first) (q::last) r
  | [t]::r -> my_flat_list first (t::last) r
  | _ -> failwith "Unsupported"

let yrange bench x_axis = match bench, x_axis with
  | "incl", "n" -> Some ("-0.001", "0.01")
  (* | "join" | "triple_join" -> Some ("0", "0.32")
  | "meet" -> Some ("0", "0.2")
  | "build_total" -> Some ("0", "1.4")
  | "incl" -> Some ("-0.02", "0.16") *)
  | _ -> None



let mk_plot
    ?(graph_style=graph_style)
    ?(output=output)
    ?(select="")
    ~y_axis ~x_axis
    ~filter
    bench
    rows
    =
  let is_latex = match output with LaTeX _ -> true | _ -> false in
  let title = make_title ~y_axis ~x_axis (String.concat "; " bench) filter is_latex graph_style in
  let bench = List.mapi (fun i x -> (i,x)) bench in
  let data = List.concat_map (fun (i,bench) ->
      let rows = List.filter (fun x -> x.name = bench && Filter_ast.eval_bexpr (fun n -> SMap.find n x.params) filter) rows in
      let data = List.fold_left (get_data y_axis x_axis) SMap.empty rows in
      let dash_type = if i = 0 then None else Some ("3") in
      SMap.fold (fun title data acc ->
        if matches_select select title
        then plot_data graph_style is_latex title dash_type data :: acc
        else acc)
      data []
    ) bench |> my_flat_list [] [] in

  (* Format.printf "%d@." (List.length rows); *)
  let plot = Gnuplot.create () in
  let x =
    if is_latex
    then Format.asprintf "\\\\scriptsize%s with %a" (latex_var x_axis) (Filter_ast.pp_bexpr latex_var) filter
    else x_axis in
  let output = match output with
    | LaTeX p ->
        Gnuplot.Output.create (`Eps_latex ("graphs/" ^ p ^ ".tex")) ~size:("8cm","6cm") (* No idea what unit this is... *)
    | PNG p ->
        Gnuplot.Output.create (`Png (results_folder ^ "/" ^ p ^ ".png")) ~size:("1000","500") (* pixels *)
    | SVG p -> Gnuplot.Output.create (`Svg (results_folder ^ "/" ^ p ^ ".svg")) ~size:("1000","500") ~params:"background \"#ffffff\""
  in
  let dollar = if is_latex then "$" else "" in
  let format = Format.asprintf "\"%s%s%%.1l%s10^{%%L}%s\""
    (if is_latex then "\\\\tiny" else "")
    dollar (if is_latex then "\\\\!\\\\cdot\\\\!" else "×") dollar in
  let custom = [
        "set key outside right center", "";
        "set size ratio 0.5", "";
        "set format x " ^format, "";
        "set xtics rotate by 45 offset -2,-1", "";
        "set style fill transparent pattern 5", "";
        (* "set key horizontal", ""; *)
      ] in
  let custom = if is_latex
    then ["set format y \"\\\\tiny$%.3f$\"", ""] @ custom
    else custom in
  let custom = match yrange (List.hd bench |> snd) x_axis with
    | None -> custom
    | Some(lo,hi) -> [Format.sprintf "set yrange [%s:%s]" lo hi, ""] @ custom in
  let y, yoffset = if is_latex then
     if x_axis = "n" then None, None else Some ("\\\\scriptsize{}" ^ label_y y_axis), Some "4"
     else Some (label_y y_axis), None in
  Gnuplot.plot_many
      ~output
      ?title
      ~labels:(Gnuplot.Labels.create ~x ?y ?yoffset ())
      ~custom
      plot data;
  Gnuplot.close plot

(** plot "empty" data (Nan) to just get the key *)
let plot_key graph_style output =
  (* let is_latex = match output with LaTeX _ -> true | _ -> false in *)
  let output = match output with
    | LaTeX _ ->
        Gnuplot.Output.create (`Eps_latex ("graphs/key.tex")) ~size:("15cm","1.5cm") (* No idea what unit this is... *)
    | PNG _ ->
        Gnuplot.Output.create (`Png (results_folder ^ "/key.png")) ~size:("1000","500") (* pixels *)
    | SVG _ -> Gnuplot.Output.create (`Svg (results_folder ^ "/new/key.svg")) ~size:("800","80") ~params:"background \"#ffffff\""
  in
  let plot = Gnuplot.create () in
  let data = List.map (fun n -> plot_data graph_style false n None [Float.nan, Float.nan, Float.nan]) all |> my_flat_list [] [] in
  Gnuplot.plot_many
    ~output
    ~custom:([
        "set key at screen 0.5, screen 0.5 center", "";
        "set key horizontal", "";
        "set xrange [0:1]", "";
        "set yrange [0:1]", "";
        "unset border", "";
        "unset tics", "";
        "unset xlabel", "";
        "unset ylabel", "";
      ] )
    plot
    data;
  Gnuplot.close plot

let plot_multi output bench rows =
  let select = "PC & !PAS & !H" in
  (* mk_plot ~select ~output:(output (List.hd bench ^ "_time_n")) ~y_axis:Time ~x_axis:"n" ~filter:"u = 30000 && d = 5000" bench; *)
  mk_plot ~select ~output:(output (List.hd bench ^ "_time_nf")) ~y_axis:Time ~x_axis:"n"
    ~filter:(let open Filter_ast in
      And(
        Binpred(Eq, Var "u", Binop(Div, Var "n", Const Testing_utils.XP_config.n_graph_udiv)),
        Binpred(Eq, Var "d", Const Testing_utils.XP_config.n_graph_d)))
    bench rows;
  mk_plot ~select ~output:(output (List.hd bench ^ "_time_d")) ~y_axis:Time ~x_axis:"d"
    ~filter:(let open Filter_ast in
      And(
        Binpred(Eq, Var "u", Const Testing_utils.XP_config.d_graph_u),
        Binpred(Eq, Var "n", Const Testing_utils.XP_config.d_graph_n)))
    bench rows
  (* mk_plot ~select:"PC" ~output:(output (bench ^ "_time_u")) ~y_axis:Time ~x_axis:"u" ~filter:"n = 200_000 && d = 2000" bench *)
  (* mk_plot ~select:"PC" ~output:(output_fmt "_mem_n") ~y_axis:Mem ~x_axis:"n" ~filter:"u = 2000 && d = 2000" bench;
  mk_plot ~select:"PC" ~output:(output_fmt "_mem_d") ~y_axis:Mem ~x_axis:"d" ~filter:"u = 5000 && n = 50000" bench;
  mk_plot ~select:"PC" ~output:(output_fmt "_mem_u") ~y_axis:Mem ~x_axis:"u" ~filter:"n = 50000 && d = 2000" bench *)

let graphs () =
  let rows = read_csv "results.csv" in
  let output s = SVG (s) in
  plot_multi output ["build_total"] rows;
  plot_multi output ["join"] rows;
  (* plot_multi output ["build_join"] rows; *)
  plot_multi output ["triple_join"] rows;
  plot_multi output ["meet"] rows;
  (* plot_multi output ["build_meet"] rows; *)
  (* plot_multi output ["triple_meet"] rows; *)
  plot_multi output ["incl";"incl_parent"] rows
  (* plot_multi output ["total"] rows; *)
  (* plot_multi output ["join_sum"] rows; *)
  (* plot_multi output ["meet_sum"] rows *)

let str_len x =  String.length (Str.global_replace (Str.regexp "µ") "m" x)

let print_table (table : string array array) : unit =
  let col_count = Array.fold_left (fun acc row -> max acc (Array.length row)) 0 table in
  let col_widths = Array.make col_count 0 in
  Array.iter (fun row ->
    Array.iteri (fun j cell -> col_widths.(j) <- max col_widths.(j) (str_len cell)) row)
  table;
  Array.iter (fun row ->
    let cells =
      Array.init col_count (fun j ->
        let cell = if j < Array.length row then row.(j) else "" in
        let pad = col_widths.(j) - str_len cell in
        if j = 0 then cell ^ String.make pad ' '
        else String.make pad ' ' ^ cell)
    in Format.printf "%s@." (String.concat " " (Array.to_list cells)))
  table

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

let unit_prefixes = ["n"; "µ"; "m"; ""; "k"; "M"; "G"; "T"; "P"; "E"; "Z"; "Y"; "R"; "Q" ]
(* let float_units = ["m"; "µ"; "n"; "p"; "f"; "a"; "z"; "y"; "r"; "q"] *)



let pp_with_unit
    ?(justify=false)
    ?(unit_prefixes=unit_prefixes)
    ?(separator="")
    ?(base=1000) () fmt nb =
  let base = float_of_int base in
  if justify then
    let str = Format.asprintf "%a" (pp_with_unit unit_prefixes separator base) nb in
    let unit_length = List.fold_left (fun x elt -> Stdlib.max x (str_len elt)) 0 unit_prefixes in
    Format.fprintf fmt "%s" (String.make ((* 3 digits + fixed point + unit max length *)4+unit_length+(String.length separator) - String.length str) ' ' ^ str)
  else pp_with_unit unit_prefixes separator base fmt nb

let table () =
  let rows = read_csv "table.csv" in
  let table = Array.make_matrix (SMap.cardinal (List.hd rows).data + 1) 5 "" in
  table.(0).(0) <- "Variant";
  table.(0).(1) <- "join";
  table.(0).(2) <- "meet";
  table.(0).(3) <- "incl (true)";
  table.(0).(4) <- "incl (false)";
  let iter_row data data_id series =
    let column = match series with
      | "join" -> 1
      | "meet" -> 2
      | "incl_parent" -> 3
      | "incl" -> 4
      | _ -> -1
    in if column < 0 then () else
    table.(data_id).(column) <- if data.time > 0. then Format.asprintf "%as" (pp_with_unit ()) (data.time *. 1e9) else "--" in
  let all = SMap.bindings (List.hd rows).data |> List.mapi (fun i (name,_) -> name,i) in
  List.iter (fun (name,i) ->
    let name, _, _ ,_ = caption name in
    table.(i+1).(0) <- name) all;
  List.iter (fun row ->
    SMap.iter (fun name data ->
      let data_id = List.assoc name all + 1 in
      iter_row data data_id row.name
    ) row.data) rows;
  print_table table



let main () =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "table" then table () else graphs()
let () = main ()
