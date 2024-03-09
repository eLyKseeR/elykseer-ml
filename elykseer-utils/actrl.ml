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
  match Environment.EnvironmentWritable.finalise_assembly actrl.env with
  | None -> Lwt_io.printlf "failed to finalise assembly"
  | Some (aid, ki) ->
    let%lwt relk = Relkeys.new_map actrl.env.econfig in
    let%lwt _ = Relkeys.add aid ki relk in
    let%lwt () = Relkeys.close_map relk in
    Lwt_io.printlf "stopping assembly controller %s" actrl.myid

let addblock actrl fn (fb : Assembly.blockinformation) buf =
  (* let%lwt () = Lwt_io.printlf "adding block (%d@%d=%d) with buffer (%d)" (Conversion.p2i fb.blockid) (Conversion.n2i fb.filepos) (Conversion.n2i fb.blocksize)
               (Cstdio.File.Buffer.size buf) in *)
  let bplain = Cstdio.BufferPlain.from_buffer buf in
  let (env', (bi, _aki)) = Environment.EnvironmentWritable.backup actrl.env fn fb.filepos bplain in
  (* let kstore = match aki with *)
  Lwt.return ({actrl with env = env'}, bi)
