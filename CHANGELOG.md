# Changelog

## [0.10.1] - 2026-10-05

### Security

- "lxr_restore" keeps restored files below the output directory (`-o`): leading `/` is dropped (an absolute name is restored relative to `-o`, like tar), names containing `..` and paths escaping through symlinks are refused
- "lxr_distribute" accepts only `https`, `http` only to localhost/loopback; credentials can be read from environment variables (`access-key-env`, `secret-key-env`); a sink configuration with inline secrets must not be readable by group or others (`chmod 600`); `--insecure` disables these checks
- "lxr_distribute" exits with code 2 on an invalid sink configuration instead of silently skipping the sink
- tests and documentation (`doc/07_Keys_and_Nonces.md`) for the uniqueness of key and GCM nonce per assembly
- Tekton secrets are kept as `*.yaml.example` templates; the filled-in files are ignored by git

### Build

- Docker images: base image pinned by version and digest, all third-party `git clone`s pinned to commits
- prebuilt Boost/zlib tarballs are verified against `docker/prebuilt/SHA256SUMS`; provenance and rebuild script in `docker/prebuilt/`
- GitHub Actions bumped and pinned to commit SHAs; semgrep runs on `ubuntu-latest`; Docker publish builds `docker/Dockerfile` with the required build context and signs with cosign v3

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