open Elykseer__Lxr

type t = {
  myid : string;
  env : Environment.EnvironmentWritable.coq_E
}

let create (config : Configuration.configuration) = Lwt.return {
  myid = config.my_id;
  env = Environment.EnvironmentWritable.initial_environment config
}

let stop actrl =
  let env = Environment.EnvironmentWritable.finalise_assembly actrl.env in
  let%lwt relk = Relkeys.new_map env.config in
  let%lwt () = Lwt_list.iter_s (fun (aid, ki) ->
                                let%lwt _ = Relkeys.add aid ki relk in Lwt.return ()) env.keys in
  let%lwt () = Relkeys.close_map relk in
  Lwt_io.printlf "stopping assembly controller %s" actrl.myid

let addblock actrl fn (fb : Assembly.blockinformation) buf =
  (* let%lwt () = Lwt_io.printlf "adding block (%d@%d=%d) with buffer (%d)" (Conversion.p2i fb.blockid) (Conversion.n2i fb.filepos) (Conversion.n2i fb.blocksize)
               (Cstdio.File.Buffer.size buf) in *)
  let bplain = Buffer.BufferPlain.from_buffer buf in
  let env' = Environment.EnvironmentWritable.backup actrl.env fn fb.filepos bplain in
  let (_fname, fb') = List.hd @@ Environment.fblocks env' in
  Lwt.return ({actrl with env = env'}, {fb' with blockid = fb.blockid})
