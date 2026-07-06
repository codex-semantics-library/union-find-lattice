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

open Testing_utils.XP_config

let nb_forks = 1

let pp_val fmt v =
  Format.fprintf fmt ",%f,%d" (Stats.average v) (Stats.standard_deviation_percent v |> int_of_float)

let pp_res fmt (res: _ Bench.bench_result) =
  pp_val fmt res.time;
  pp_val fmt res.allocated_bytes
  (* pp_val fmt res.major_words;
  pp_val fmt res.promoted_words *)

let columns = [
  "time (s)";
  "alloc (b)";
  (* "major words (w)";
  "promoted words (w)"; *)
]
let std_dev_header = "+/-%"
let column_headers = String.concat ("," ^ std_dev_header ^ ",") columns ^ "," ^ std_dev_header
let column_meta_header = String.make (2 * List.length columns - 1) ','



type param = int * int * int

type result = Bench.run_result list * Bench.gen_info

let run_bench (simple, nb_nodes, nb_ops, nb_branch_ops, nb_iterations) suite =
  Bench.repeat_runs ~simple nb_iterations ~nb_nodes ~nb_ops ~nb_branch_ops suite

(* Split a list into n sublists of roughly equal size *)
let rec split_list output i n = function
  | [] -> Array.to_list output
  | x::xs ->
      output.(i) <- x::output.(i);
      let j = i + 1 in
      split_list output (if j >= n then 0 else j) n xs
let split_list n lst = split_list (Array.make n []) 0 n lst

let pp_headers fmt suites =
  let parameters = [
    "n"; "u"; "d"; "k"; "nb_cls"; "sz_cls"; "rk_cls"; "dup_u"
  ] in
  Format.fprintf fmt ",Parameters%s" (String.make (List.length parameters - 1) ',' );
  List.iter (fun suite -> Format.fprintf fmt ",%s%s" suite.Testing_utils.Instances.name column_meta_header) suites;
  Format.fprintf fmt "@.bench,%s" (String.concat "," parameters);
  List.iter (fun _ -> Format.fprintf fmt ",%s" column_headers) suites;
  Format.fprintf fmt "@."

let write_results print_headers path res suite =
  let outc = if path = "" then stdout else open_out path in
  let fmt = Format.formatter_of_out_channel outc in
  if print_headers then pp_headers fmt suite;
  List.iter (fun ((_,nb_nodes, nb_ops, nb_branch_ops,k),(res, gen)) ->
    let pp name list =
      Format.fprintf fmt "%s,%d,%d,%d,%d,%.0f,%.0f,%.0f,%.0f" name nb_nodes nb_ops nb_branch_ops k
        (Stats.average gen.Bench.nb_classes |> Float.round)
        (Stats.average gen.Bench.max_class_size |> Float.round)
        (Stats.average gen.Bench.max_class_rank |> Float.round)
        (Stats.average gen.Bench.nb_duplicate_unions |> Float.round)
        ;
      List.iter (pp_res fmt) list;
      Format.pp_print_newline fmt () in
    let open Bench in
    if path <> "" then begin
      pp "make" (List.map (fun x -> x.init) res);
      pp "build_common" (List.map (fun x -> x.build_common) res);
      pp "build_branches" (List.map (fun x -> x.build_branches) res);
      pp "build_total" (List.map (fun x -> x.build) res);
      (* pp "find" (List.map (fun x -> x.find) res); *)
      (* pp "union" (List.map (fun x -> x.union) res); *)

      pp "join" (List.map (fun x -> x.join) res);
      (* pp "join_with_parent" (List.map (fun x -> x.join_root) res); *)
      (* pp "double_join" (List.map (fun x -> x.double_join) res); *)
      pp "triple_join" (List.map (fun x -> x.triple_join) res);
      (* pp "join_sum" (List.map (fun x -> x.join_all) res); *)
      pp "build_join" (List.map (fun x -> x.build_join) res);

      pp "meet" (List.map (fun x -> x.meet) res);
      (* pp "meet_with_parent" (List.map (fun x -> x.meet_root) res); *)
      (* pp "double_meet" (List.map (fun x -> x.double_meet) res); *)
      pp "triple_meet" (List.map (fun x -> x.double_meet) res);
      (* pp "meet_sum" (List.map (fun x -> x.meet_all) res); *)
      pp "build_meet" (List.map (fun x -> x.build_meet) res);

      pp "incl" (List.map (fun x -> x.incl) res);
      pp "incl_parent" (List.map (fun x -> x.incl_root) res);

      pp "total" (List.map (fun x -> x.total) res)
    end else begin
      pp "join" (List.map (fun x -> x.join) res);
      pp "meet" (List.map (fun x -> x.meet) res);
      pp "total" (List.map (fun x -> x.total) res)
    end
  ) res;
  if path = "" then () else close_out outc

let worker_body ~print_headers outfile sublist suite =
  let results = ref [] in
  Sys.catch_break true;
  (* use list.iter and not list.map to keep the result in case of exception *)
  (try List.iter (fun ((_,n,u,d,k) as param) ->
    let begin_t = (Unix.times ()).tms_utime in
    results := (param, run_bench param suite)::!results;
    let t = (Unix.times ()).tms_utime -. begin_t in
    Format.printf "DONE N=%d, U=%d, d=%d; Time: %.5f@." n u d t
  ) sublist
  with Sys.Break -> ());
  write_results print_headers outfile !results suite

(* Run benchmarks in a single fork, writing results to a CSV file *)
let fork_worker ~print_headers outfile suite sublist =
  let pid = Unix.fork () in
  if pid = 0 then begin
    (* Child process, write partial result in temporary file. *)
    let outfile = if outfile = "" then "" else Printf.sprintf "%s_%d.csv" outfile (Unix.getpid ()) in
    worker_body ~print_headers outfile sublist suite;
    exit 0
  end else pid



let run_parallel ~nb_forks outfile inputs suite =
  Sys.catch_break true;
  if nb_forks <= 1 then begin
    worker_body ~print_headers:true outfile inputs suite;
    Format.printf "Wrote results to %s@." outfile
  end else begin
  let workers = split_list nb_forks inputs |> List.map (fork_worker ~print_headers:false outfile suite) in
  begin try List.iter (fun pid -> try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ()) workers
  with Sys.Break -> Unix.sleep 2 end;

  if outfile <> "" then
    (* Collect whichever CSV files exist (some may be partial if interrupted) *)
    let csv_files =
      List.filter_map (fun pid ->
        let path = Printf.sprintf "%s_%d.csv" outfile pid in
        if Sys.file_exists path then Some path else None
      ) workers
    in
    (* Merge output files into one *)
    let outc = open_out outfile in
    let fmt = Format.formatter_of_out_channel outc in
    pp_headers fmt suite;
    List.iter (fun file ->
      let inc = open_in file in
      (try while true do
        output_string outc (input_line inc);
        output_char outc '\n'
      done with End_of_file -> ());
      close_in inc;
      Sys.remove file) csv_files;
    close_out outc;
    Format.printf "Wrote results to %s@." outfile
    end

let time () =
  let inputs = List.filteri (fun i _ -> i <= 20) graph_worklist in
  (* Time a single-domain run vs multi-domain *)
  let t0 = Unix.gettimeofday () in
  let _ = run_parallel ~nb_forks:1 "test.csv" inputs graph_suite in
  Format.printf "1 domain:  %.3fs@." (Unix.gettimeofday () -. t0);

  let t1 = Unix.gettimeofday () in
  let _ = run_parallel ~nb_forks:10 "test.csv" inputs graph_suite in
  Format.printf "10 domains: %.3fs@." (Unix.gettimeofday () -. t1)

let bench suite work_list outfile = run_parallel ~nb_forks outfile work_list suite

let test_size = 10000
let bias = [0.0;0.1; 0.2; 0.3; 0.4; 0.5; 0.6]
let nb_ops_1 = [10;20;100;1000;2500;5000]
let nb_ops_2 = [1000;5000;8000;10000;12000;15000;20000]

let gen_stats () =
  Format.printf "With bias, nb_nodes=%d@." test_size;
  List.iter (fun bias ->
    List.iter (fun nb_ops ->
      let _, uf = Gen.gen1 ~nb_ops ~bias ~nb_nodes:test_size ~union_ratio:1. in
      Format.printf "Ops: %4d, bias: %.1f;    %a" nb_ops bias Gen.UF.pp_stats uf
      ) nb_ops_1;
  ) bias;

  Format.printf "@.@.Pure random, nb_nodes=%d@." test_size;
  List.iter (fun nb_ops ->
      let _, m, uf = Gen.gen2 ~nb_ops ~nb_nodes:test_size in
      Format.printf "Ops: %5d; double-unions: %5d   %a" nb_ops m Gen.UF.pp_stats uf
    ) nb_ops_2

let pp_gc = function
  | Testing_utils.XP_config.Always -> "Always"
  | OnBranch -> "OnBranch"
  | Once -> "Once"
  | Never -> "Never"

let test_configs () =
  let gcs = Bench.[Always; OnBranch; Once; Never] in
  let test = [(false, 200_000, 2000, 2000, 10); (false, 200_000, 2000, 2000, 50); (false, 200_000, 2000, 2000, 100);] in
  Format.printf "Single fork:@.";
  List.iter (fun gc ->
    Format.printf "%s@." (pp_gc gc);
    run_parallel ~nb_forks:1 "" test graph_suite) gcs;

  Format.printf "@.8 forks:@.";
  let test = List.init 8 (fun _ -> test) |> List.flatten in
  List.iter (fun gc ->
    Format.printf "%s@." (pp_gc gc);
    run_parallel ~nb_forks:8 "" test graph_suite) gcs

let main () =
  if Array.length Sys.argv > 1 then
    if Sys.argv.(1) = "gen_stats"
    then gen_stats ()
    else if Sys.argv.(1) = "configs"
    then test_configs ()
    else if Sys.argv.(1) = "table"
    then bench sample_suite sample_worklist sample_outfile
    else failwith "Unknown args"
  else bench graph_suite graph_worklist graph_outfile

let () = main ()
