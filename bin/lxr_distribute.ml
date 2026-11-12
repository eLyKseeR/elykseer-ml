(* copies the chunks of an assembly to sinks (PUT) or back (GET);
   the plan, the copies and the retries are Coq code (theories/Distribution.v) *)

open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

(* example:
   dune exec bin/lxr_distribute.exe -- -v -d PUT -n 16 -x ../elykseer.chunks -i $MYID -a $AID -c sinks.json 4 99
*)

let arg_verbose = ref false
let arg_weights : int list ref = ref []
let arg_aid = ref ""
let arg_chunkpath = ref ""
let arg_configpath = ref ""
let arg_nchunks : int ref = ref 16
let arg_myid = ref Cli.default_myid
let arg_direction = ref "PUT"  (* or GET *)
let arg_insecure = ref false

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-d", Arg.Set_string arg_direction, "sets direction of copy: GET | PUT (default)");
    ("-a", Arg.Set_string arg_aid, "sets assembly id");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-n", Cli.nchunks_spec arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
    ("-c", Arg.Set_string arg_configpath, "sets path for sink configuration file");
    ("--insecure", Arg.Set arg_insecure, "allows http to non-local hosts and a sink configuration with inline secrets that is readable by group or others");
  ]

let usage_msg = "lxr_distribute -a aid -x chunkpath -c sinks.json [-d GET|PUT] [-n nchunks] [-i myid] [-v] [--insecure] <weight1> [<weight2>] ..."

let anon_args_fun fn =
  match int_of_string_opt fn with
  | Some w when w >= 0 -> arg_weights := w :: !arg_weights
  | _ -> raise (Arg.Bad (Printf.sprintf "weight must be a non-negative integer: '%s'" fn))

let sink_label = function
  | Distribution.FS s -> "FS " ^ s.name
  | Distribution.S3 s -> "S3 " ^ s.name

let result_label = function
  | Distribution.Copied -> "copied"
  | Distribution.Present -> "present"
  | Distribution.Retry c -> Printf.sprintf "retry %d" (Conversion.z2i c)
  | Distribution.Failed c -> Printf.sprintf "failed %d" (Conversion.z2i c)

let main () = Arg.parse argspec anon_args_fun usage_msg;
  Cli.require argspec usage_msg [("-a", !arg_aid); ("-x", !arg_chunkpath); ("-c", !arg_configpath)];
  if !arg_weights = [] then Cli.usage_error argspec usage_msg "missing weights: one per sink";
  let direction = match !arg_direction with
    | "PUT" -> Distribution.PUT
    | "GET" -> Distribution.GET
    | _ -> Cli.usage_error argspec usage_msg "-d must be GET or PUT" in
  Cli.warn_default_myid !arg_myid;
  let nchunks = Nchunks.from_int !arg_nchunks in
  let tracer = if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning in
  let conf : configuration = {
                  config_nchunks = nchunks;
                  path_chunks = !arg_chunkpath;
                  path_db     = Filename.concat (Filename.get_temp_dir_name ()) "db";
                  my_id       = !arg_myid;
                  trace       = tracer } in
  let jconf = Yojson.Basic.from_file !arg_configpath in
  let inline_secrets = Sinkconfig.has_inline_secrets jconf in
  let () = match Sinkcheck.check_config_mode ~insecure:!arg_insecure ~inline_secrets !arg_configpath with
    | Ok () -> ()
    | Error msg -> Printf.eprintf "%s\n" msg; exit 2 in
  let osinks = Sinkconfig.from_json_sinks ~insecure:!arg_insecure conf jconf in
  let () = if List.exists Option.is_none osinks then begin
      Printf.eprintf "invalid sink configuration in '%s'\n" !arg_configpath; exit 2 end in
  let sinks = List.filter_map Fun.id osinks in
  let keys = Distribution.enumerate_chunk_keys conf !arg_aid nchunks in
  let ws = List.rev_map Conversion.i2n !arg_weights in
  (* a sum above nchunks stores chunks on more than one sink *)
  match Distribution.plan sinks ws keys with
  | None ->
    Cli.usage_error argspec usage_msg
      (Printf.sprintf "need one weight per sink (%d weights for %d sinks) summing up to at least the %d chunks (sum %d)"
         (List.length ws) (List.length sinks) !arg_nchunks (List.fold_left (+) 0 !arg_weights))
  | Some plan ->
    List.iteri (fun i (s, ks) ->
        Printf.printf "%d : %s, %d chunks\n" (i + 1) (sink_label s) (List.length ks);
        if !arg_verbose then List.iteri (fun j k -> Printf.printf "  %d: %s\n" (j + 1) k) ks)
      plan;
    let results = Distribution.run direction plan in
    List.iter (fun (f : _ Distribution.final) ->
        let ok = Distribution.is_ok f.f_result in
        if !arg_verbose || not ok then
          Printf.printf "%s %s: %s after %d attempt%s\n" (sink_label f.f_sink) f.f_key
            (result_label f.f_result) (Sinkconfig.int_of_nat f.f_attempts) (if Sinkconfig.int_of_nat f.f_attempts = 1 then "" else "s"))
      results;
    (* PUT: every planned copy; GET: every chunk from at least one sink *)
    if Distribution.run_ok direction keys results then 0
    else begin
      let nfailed = List.length (List.filter (fun (f : _ Distribution.final) -> not (Distribution.is_ok f.f_result)) results) in
      Printf.eprintf "distribution incomplete: %d cop%s failed\n" nfailed (if nfailed > 1 then "ies" else "y");
      1
    end

let () = exit (main ())
