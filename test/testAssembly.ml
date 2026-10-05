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


(* the C++ glue rejects a copy beyond the target buffer; this used to be
   reported as 0 bytes written *)
let test_add_content_error () =
  let content = Cstdio.File.Buffer.create 32 in
  let buffer = Cstdio.File.Buffer.create (16*256*1024) in
  Alcotest.check_raises "beyond the target"
    (Assembly.Content_error "add_content of 32 bytes at 4194300 failed: target buffer too small (-2)")
    (fun () -> ignore (Testing.add_content content 32 (16*256*1024 - 4) buffer));
  Alcotest.check_raises "size 0"
    (Assembly.Content_error "add_content of 0 bytes at 0 failed: size < 1 (-3)")
    (fun () -> ignore (Testing.add_content content 0 0 buffer))

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

let flip_byte buf i =
  let c = Cstdio.File.Buffer.get buf i in
  Cstdio.File.Buffer.set buf i (Char.chr (((Char.code c) lxor 0x80) land 0xff))

let test_gcm_tampered_tag () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  let buf = enc_buffer be in
  (* the tag is stored in the last tag_len bytes *)
  flip_byte buf (Cstdio.File.Buffer.size buf - 1);
  Alcotest.(check bool) "tampered tag fails" true
    (Option.is_none (L.Assembly.decrypt ae be ki))

let test_gcm_tampered_first () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  flip_byte (enc_buffer be) 0;
  Alcotest.(check bool) "tampered first byte fails" true
    (Option.is_none (L.Assembly.decrypt ae be ki))

let test_gcm_wrong_ivec () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  let ki' = { ki with L.Assembly.ivec = Elykseer_crypto.Key128.mk () |> Elykseer_crypto.Key128.to_hex } in
  Alcotest.(check bool) "wrong ivec fails" true
    (Option.is_none (L.Assembly.decrypt ae be ki'))

let test_gcm_truncated () =
  let ki = gcm_keyinfo () in
  let ae, be, _ = gcm_encrypted ki in
  let s = enc_buffer be |> Cstdio.File.Buffer.to_string in
  let truncated n =
    Cstdio.File.Buffer.from_string (String.sub s 0 (String.length s - n))
    |> L.Cstdio.BufferEncrypted.from_buffer |> L.Assembly.id_enc_from_buffer_t in
  Alcotest.(check bool) "missing tag fails" true
    (Option.is_none (L.Assembly.decrypt ae (truncated (L.Conversion.n2i L.Cstdio.tag_len)) ki));
  Alcotest.(check bool) "missing chunk fails" true
    (Option.is_none (L.Assembly.decrypt ae (truncated (L.Conversion.n2i L.Assembly.chunksize_N)) ki))

let test_restore_wrong_checksum () =
  let ki = gcm_keyinfo () in
  let ae, be, bi = gcm_encrypted ki in
  match L.Assembly.decrypt ae be ki with
  | None -> Alcotest.fail "decryption failed"
  | Some (_, bd) ->
    let bi' = { bi with L.Assembly.bchecksum = String.make 64 '0' } in
    Alcotest.(check bool) "block with wrong checksum is not restored" true
      (Option.is_none (L.Assembly.restore bd bi'))

(* random header: nchunks - tag_len bytes, at most 128; nchunks >= 145 overflowed before *)
let test_header_size () =
  List.iter (fun (n, expected) ->
      let c = { (gcm_config ()) with L.Configuration.config_nchunks = L.Nchunks.from_int n } in
      let a, _ = L.Assembly.AssemblyPlainWritable.create c in
      Alcotest.(check int) (Printf.sprintf "header for %d chunks" n) expected (L.Conversion.n2i (L.Assembly.apos a)))
    [(16, 0); (32, 16); (144, 128); (145, 128); (256, 128)]

(* fill an assembly through EnvironmentWritable.backup until it is finalised,
   then every block must restore; with 1001-byte blocks the last block
   reaches into the last rows of the assembly, whose bytes in the last
   chunk share the physical position of the GCM tag *)
let test_full_assembly_restores () =
  let chunkdir = Filename.temp_dir "lxr_test_chunks" "" in
  let conf = { (gcm_config ()) with L.Configuration.path_chunks = chunkdir } in
  let content i = String.init 1001 (fun j -> Char.chr ((i * 31 + j * 7) land 0xff)) in
  let rec fill e i bis =
    let b = L.Cstdio.BufferPlain.from_buffer (Cstdio.File.Buffer.from_string (content i)) in
    let e', (bi, ki) = L.Environment.EnvironmentWritable.backup e "f" (L.Conversion.i2n (i * 1001)) b in
    match ki with
    | Some (aid, ki) -> (aid, ki, List.rev bis)   (* bi went into the next assembly *)
    | None -> fill e' (i + 1) ((i, bi) :: bis) in
  let aid, ki, bis = fill (L.Environment.EnvironmentWritable.initial_environment conf) 0 [] in
  let e0 = L.Environment.EnvironmentReadable.initial_environment conf in
  let nfailed = match L.Environment.EnvironmentReadable.restore_assembly e0 aid ki with
    | None -> Alcotest.fail "cannot restore assembly"
    | Some e ->
      List.filter (fun (i, bi) ->
          match L.Assembly.restore (L.Environment.cur_buffer e) bi with
          | None -> true
          | Some b -> L.Cstdio.BufferPlain.to_buffer b |> Cstdio.File.Buffer.to_string <> content i) bis
      |> List.length in
  ignore (Sys.command (Filename.quote_command "rm" ["-rf"; chunkdir]));
  Alcotest.(check int) (Printf.sprintf "all %d blocks of a full assembly restore" (List.length bis)) 0 nfailed

(* GCM nonce = first 12 bytes (24 hex chars) of the ivec *)
let gcm_nonce (ki : L.Assembly.keyinformation) = String.sub ki.ivec 0 24

let all_distinct ls = List.length (List.sort_uniq compare ls) = List.length ls

(* the key generators used by finalise_assembly never repeat a key or a nonce *)
let test_key_uniqueness () =
  let n = 1000 in
  let kis = List.init n (fun _ ->
      { L.Assembly.ivec = L.Environment.cpp_mk_key128 ()
      ; pkey = L.Environment.cpp_mk_key256 ()
      ; localnchunks = L.Conversion.i2p 16 }) in
  Alcotest.(check int) "pkey length" 64 (String.length (List.hd kis).pkey);
  Alcotest.(check int) "ivec length" 32 (String.length (List.hd kis).ivec);
  Alcotest.(check bool) "keys distinct" true
    (all_distinct (List.map (fun (ki : L.Assembly.keyinformation) -> ki.pkey) kis));
  Alcotest.(check bool) "nonces distinct" true
    (all_distinct (List.map gcm_nonce kis))

(* two finalised assemblies never share aid, key or nonce *)
let test_finalise_fresh_keys () =
  let chunkdir = Filename.temp_dir "lxr_test_chunks" "" in
  let conf = { (gcm_config ()) with L.Configuration.path_chunks = chunkdir } in
  let finalise () =
    let e0 = L.Environment.EnvironmentWritable.initial_environment conf in
    let content = L.Cstdio.BufferPlain.from_buffer (Cstdio.File.Buffer.from_string gcm_content) in
    let e1, _ = L.Environment.EnvironmentWritable.backup e0 "testfile" (L.Conversion.i2n 0) content in
    match L.Environment.EnvironmentWritable.finalise_assembly e1 with
    | None -> Alcotest.fail "finalise_assembly failed"
    | Some (aid, ki) -> (aid, ki) in
  let (aid1, ki1) = finalise () in
  let (aid2, ki2) = finalise () in
  ignore (Sys.command (Filename.quote_command "rm" ["-rf"; chunkdir]));
  Alcotest.(check bool) "aids differ" true (aid1 <> aid2);
  Alcotest.(check bool) "keys differ" true (ki1.pkey <> ki2.pkey);
  Alcotest.(check bool) "nonces differ" true (gcm_nonce ki1 <> gcm_nonce ki2)

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
    test_case "add content error" `Quick test_add_content_error;
    test_case "header size" `Quick test_header_size;
    test_case "gcm roundtrip" `Quick test_gcm_roundtrip;
    test_case "gcm encrypted size" `Quick test_gcm_size;
    test_case "gcm wrong key" `Quick test_gcm_wrong_key;
    test_case "gcm wrong aid" `Quick test_gcm_wrong_aid;
    test_case "gcm tampered" `Quick test_gcm_tampered;
    test_case "gcm tampered tag" `Quick test_gcm_tampered_tag;
    test_case "gcm tampered first byte" `Quick test_gcm_tampered_first;
    test_case "gcm wrong ivec" `Quick test_gcm_wrong_ivec;
    test_case "gcm truncated" `Quick test_gcm_truncated;
    test_case "restore wrong checksum" `Quick test_restore_wrong_checksum;
    test_case "full assembly restores" `Quick test_full_assembly_restores;
    test_case "key and nonce uniqueness" `Quick test_key_uniqueness;
    test_case "finalise uses fresh keys" `Quick test_finalise_fresh_keys;
    test_case "tracer disabled level" `Quick test_tracer_disabled_level;
  ]