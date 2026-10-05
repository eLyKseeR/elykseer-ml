(* open Elykseer__Lxr *)
open Elykseer_base
open Mlcpp_cstdio

module Testing = struct
  let add_content = fun bsrc sz pos btgt -> Assembly.add_content ~src:bsrc ~sz:sz ~pos:pos ~tgt:btgt

end

(* Tests *)

let test_add_content () =
  let msg = "testing" in
  let content = Cstdio.File.Buffer.init 32 (fun i -> if i < 7 then String.get msg i else '0') in
  let buffer = Cstdio.File.Buffer.create (16*256*1024) in
  Alcotest.(check int) "add content"
  4 (* == *) (Testing.add_content content 4 0 buffer)


(* AES-256-GCM encryption of assemblies *)

module L = Elykseer__Lxr

let gcm_config () : L.Configuration.configuration =
  { L.Configuration.config_nchunks = L.Nchunks.from_int 16
  ; path_chunks = "/tmp/lxr_test_chunks"
  ; path_db = "/tmp/lxr_test_db"
  ; my_id = "1234567890"
  ; trace = L.Tracer.nullTracer }

let gcm_keyinfo () : L.Assembly.keyinformation =
  { L.Assembly.ivec = Elykseer_crypto.Key128.mk () |> Elykseer_crypto.Key128.to_hex
  ; pkey = Elykseer_crypto.Key256.mk () |> Elykseer_crypto.Key256.to_hex
  ; localnchunks = L.Conversion.i2p 16 }

let gcm_content = String.init 1000 (fun i -> Char.chr (i mod 256))

(* create an assembly, backup some content, and encrypt it *)
let gcm_encrypted ki =
  let a0, b0 = L.Assembly.AssemblyPlainWritable.create (gcm_config ()) in
  let content = L.Cstdio.BufferPlain.from_buffer (Cstdio.File.Buffer.from_string gcm_content) in
  let a1, bi = L.Assembly.backup a0 b0 (L.Conversion.i2n 0) content in
  let a2, b2 = L.Assembly.finish a1 b0 in
  match L.Assembly.encrypt a2 b2 ki with
  | None -> Alcotest.fail "encryption failed"
  | Some (ae, be) -> (ae, be, bi)

let enc_buffer be =
  L.Assembly.id_buffer_t_from_enc be |> L.Cstdio.BufferEncrypted.to_buffer

let test_gcm_roundtrip () =
  let ki = gcm_keyinfo () in
  let ae, be, bi = gcm_encrypted ki in
  match L.Assembly.decrypt ae be ki with
  | None -> Alcotest.fail "decryption failed"
  | Some (_, bd) ->
    match L.Assembly.restore bd bi with
    | None -> Alcotest.fail "restore failed"
    | Some b ->
      Alcotest.(check string) "restored content" gcm_content
        (L.Cstdio.BufferPlain.to_buffer b |> Cstdio.File.Buffer.to_string)

let test_gcm_size () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  Alcotest.(check int) "encrypted size equals assembly size"
    (L.Assembly.assemblysize (L.Assembly.nchunks ae) |> L.Conversion.n2i)
    (enc_buffer be |> Cstdio.File.Buffer.size)

let test_gcm_wrong_key () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  let ki' = { ki with L.Assembly.pkey = Elykseer_crypto.Key256.mk () |> Elykseer_crypto.Key256.to_hex } in
  Alcotest.(check bool) "wrong key fails" true
    (Option.is_none (L.Assembly.decrypt ae be ki'))

let test_gcm_wrong_aid () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  let ae' = { ae with L.Assembly.aid = "0" ^ L.Assembly.aid ae } in
  Alcotest.(check bool) "wrong aid fails" true
    (Option.is_none (L.Assembly.decrypt ae' be ki))

let test_gcm_tampered () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  let buf = enc_buffer be in
  let c = Cstdio.File.Buffer.get buf 4711 in
  Cstdio.File.Buffer.set buf 4711 (Char.chr (((Char.code c) lxor 0x01) land 0xff));
  Alcotest.(check bool) "tampered ciphertext fails" true
    (Option.is_none (L.Assembly.decrypt ae be ki))

(* a disabled log level must not skip the traced computation *)
let test_tracer_disabled_level () =
  let run t = L.Tracer.conditionalTrace t true
                L.Tracer.Coq_debug (Some "debug message") (fun () -> Some 42)
                L.Tracer.Coq_debug None (fun () -> None) in
  Alcotest.(check (option int)) "warning tracer" (Some 42) (run L.Tracer.stdoutTracerWarning);
  Alcotest.(check (option int)) "null tracer" (Some 42) (run L.Tracer.nullTracer)

(* Runner *)

let test =
  let open Alcotest in
  "LXR Assembly",
  [
    test_case "add content" `Quick test_add_content;
    test_case "gcm roundtrip" `Quick test_gcm_roundtrip;
    test_case "gcm encrypted size" `Quick test_gcm_size;
    test_case "gcm wrong key" `Quick test_gcm_wrong_key;
    test_case "gcm wrong aid" `Quick test_gcm_wrong_aid;
    test_case "gcm tampered" `Quick test_gcm_tampered;
    test_case "tracer disabled level" `Quick test_tracer_disabled_level;
  ]