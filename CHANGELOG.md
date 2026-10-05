# Changelog

## [0.10.1] - 2026-10-05

### Security

- "lxr_restore" keeps restored files below the output directory (`-o`): leading `/` is dropped (an absolute name is restored relative to `-o`, like tar), names containing `..` and paths escaping through symlinks are refused
- "lxr_distribute" accepts only `https`, `http` only to localhost/loopback; credentials can be read from environment variables (`access-key-env`, `secret-key-env`); a sink configuration with inline secrets must not be readable by group or others (`chmod 600`); `--insecure` disables these checks
- "lxr_distribute" exits with code 2 on an invalid sink configuration instead of silently skipping the sink
- tests and documentation (`doc/07_Keys_and_Nonces.md`) for the uniqueness of key and GCM nonce per assembly
- the assembly id (aid) is derived from 256 random bits instead of 32 bits plus time and host name; a collision would have replaced the key of an earlier assembly
- C glue (`elykseer-base/assembly.cxx`): the error paths returned without `CAMLreturn`, which corrupted the GC roots and hung the program at the next garbage collection

### Fix

- **data loss:** the GCM tag could overwrite up to 15 bytes of a block in the last rows of a full assembly (data is striped over the chunks, the tag is in the last 16 bytes of the last chunk); the last 16 rows (16 × nchunks bytes) are now reserved. Affected files of 0.10.0 archives fail to restore with a checksum error, see `doc/09_Operations.md`
- "lxr_backup" crashed with a stack overflow for 145 to 256 chunks per assembly (`-n`); the random header is limited to 128 bytes. `Conversion.i2n`/`i2p` raise `Invalid_argument` on negative input instead of looping
- "lxr_backup" returns exit code 1 if a file could not be read or backed up, or if a block refers to an assembly without key; a missing file no longer aborts the run with an exception
- "lxr_distribute" returns exit code 1 if any chunk failed; result codes are negative on failure; HTTP errors on GET are no longer written as chunk files, short chunks are removed; GET into a new chunk directory creates the subdirectories
- copying data into or out of an assembly reported a rejected copy as 0 bytes written; it now raises `Elykseer_base.Assembly.Content_error`, the file is reported as failed (exit code 1)
- errors when storing keys or file meta data (`Relkeys.add`, `Relfiles.add`) were only printed; they are returned as `result` and make "lxr_backup" exit with code 1
- "lxr_restore", "lxr_relfiles" and "lxr_relkeys" refuse a database path without database (exit code 2) instead of silently creating an empty one; "lxr_backup" notes when it creates a new database

### Change

- "lxr_backup" and "lxr_restore" require `-x` and `-d`, "lxr_relfiles" and "lxr_relkeys" require `-d` (the database used to default to a temp directory, where keys could be lost); "lxr_distribute" requires `-a`, `-x`, `-c` and weights; all tools warn when the default identifier is used and show a proper usage line
- "lxr_distribute": removed option `-j`, it had no effect
- `-n` outside 16–256 is a usage error (exit 2) in "lxr_backup", "lxr_restore", "lxr_distribute" and "lxr_chunks"; it was silently clamped before
- the sink configuration parser moved to `Elykseer_utils.Sinkconfig`
- "lxr_restore --verify": restores into a temporary directory and compares each file with its checksum from the meta data; without file names all files of the identifier are checked (finds files damaged by the 0.10.0 bug)
- `Relfiles.hashes` lists all file hashes of an identifier
- removed unused code: `Elykseer_utils.Utils` (with the broken JSON of `as2j`), `Actrl`, `Zip`
- `Env.consolidate_files` groups blocks by file in one pass (was quadratic)

### Documentation

- `doc/08_Format.md`: assembly layout, encryption, identifiers, meta data database
- `doc/09_Operations.md`: what to back up, protecting the key database, restore drills, exit codes, failure modes, upgrading from 0.9.x and 0.10.0
- README: Docker quickstart, threat model, current usage of the tools; removed the section about "lxr_compare" (no longer exists); documentation URL in `dune-project`

### Test

- unit tests for decryption failures (tag, first byte, wrong ivec, truncated), header size, full assembly restore, `Env.consolidate_files`, `Fsutils`, sink configuration, `Relutils`, `Relfiles`
- command line tests (`test/test_cli.sh`, run by `dune test`): exit codes, no partial file after a failed restore, incremental backup, distribute PUT/GET
- coverage with bisect_ppx: `bash test/coverage.sh`; CI (Forgejo IT) runs `dune test` and the coverage report
- `test/test_incremental.sh` repaired (used the removed `lxr_incremental` and the removed field `localid`)
- `EXAMPLES.md` is executable: its `sh` blocks are run by `test/test_examples.sh` in the IT workflows

### Build

- removed the Tekton pipeline (`.tekton/`), it is no longer used
- Docker images: base image pinned by version and digest, all third-party `git clone`s pinned to commits
- prebuilt Boost/zlib tarballs are verified against `docker/prebuilt/SHA256SUMS`; provenance and rebuild script in `docker/prebuilt/`
- GitHub Actions bumped and pinned to commit SHAs; semgrep runs on `ubuntu-latest`; Docker publish builds `docker/Dockerfile` with the required build context and signs with cosign v3
- GitHub workflows CI (proofs, extraction check) and IT (tests, coverage, examples, roundtrip, verify), mirroring Forgejo; the CI image is pinned by digest in both
- Dockerfiles: `CMD` in JSON form; `bisect_ppx` in the base image
- the main image contains only a shallow clone of the built commit; before, the whole `.git` of the build machine (all branches, reflogs, remotes) was in an image layer
- `docker/docker-bake.hcl` builds base and main image together

## [0.10.0] - 2026-10-04

### Change

- assemblies are encrypted with AES-256-GCM instead of AES-256-CBC; the last 16 bytes of an assembly hold the authentication tag
- the assembly id (aid) is bound to the encryption as additional authenticated data (AAD)
- the GCM nonce is the first 12 bytes of the 16-byte ivec; the key information format is unchanged
- the random header of an assembly is now up to 128 bytes (as in elykseer-rs)

### Fix

- decryption fails explicitly on a wrong key, a wrong aid, or tampered/corrupted data, instead of returning garbage
- log levels disabled in the tracer no longer skip the traced computation; "lxr_backup" without "-v" did not write any chunks
- "lxr_restore" checks that a file was completely restored; an incomplete file is removed, the failure is reported, and the exit code is 1
- the Relkeys tests are run again (Alcotest.run exited the process before)

### Incompatible

- archives written by 0.9.x (AES-256-CBC) cannot be decrypted by this version
- requires elykseer-crypto with module Aes256gcm

## [0.9.16] - 2026-10-03

### Change

- upgraded to OCaml 5.2.1
- upgraded to Rocq 9.0.0

## [0.9.15] - 2025-04-18

### Added

- export metadata as XML in utilities "lxr_relfiles" and "lxr_relkeys"
- provided XML schema for validating XML data


## [0.9.14] - 2024-11-05

### Change

- field "localid" removed from structure "keyinformation"
- the utility "lxr_relfiles" now outputs file meta data in CSV format


## [0.9.13] - 2024-07-14

### Fix

- adapt Dockerfile


## [0.9.12] - 2024-05-24

### Added

- tracing messages to log output
- binary-only Docker image
- enable deduplication in backup of directory (_lxr\_backup_ flag '-D')

### Fix

- fixing a bug in dependency _ml-cpp-filesystem_ that prevented restoring files in subdirectories


## [0.9.11] - 2024-05-13

### Added

- deduplication at file level, checked by comparing file checksums, and at level of single block, checked by comparing block's checksum, vs. meta data


## [0.9.10] - 2024-04-30

### Added

- **Breaking:** _fileinformation_ now contains new field _fhash_
- all identifiers depend on _myid_


## [0.9.9] - 2024-02-18

### Fix

Simplified code base and updated documentation


## [0.9.8] - 2024-02-04

### Added

Processor for read and write requests towards a cache


## [0.9.7] - 2024-02-03

### Added

Key-value store: _KeyListStore_ and _FBlockListStore_


## [0.9.6] - 2024-01-29

### Added

_EnvironmentWritable_ and _EnvironmentReadable_


## [0.9.5] - 2024-01-14

_Started the changelog with this version_

### Added

- [CHANGELOG.md](CHANGELOG.md) was added

### Fix

- **Breaking:** Configuration.my_id is now a string so it allows for more readable identification. ([b99d286](https://github.com/eLyKseeR/elykseer-ml/commit/b99d286df15f345d6029c998d5fc7f8a4cebba53))


----

[0.9.5]: https://github.com/eLyKseeR/elykseer-ml/releases/tag/v0.9.5