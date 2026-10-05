# eLyKseeR

This project is about developing the cryptographic data archive _eLyKseeR_ using formal methods and implementing it in the functional language _Rocq_ (former _Coq_).

Copyright (c) 2026 Alexander Diemand

## Quickstart (Docker)

Back up a file from the current directory and restore it again:

```sh
docker run --rm -it -v "$PWD:/data" -w /data codieplusplus/elykseer-ml:latest bash
# inside the container:
MYID=$(head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n')
lxr_backup  -x ~/lxr.chunks -d ~/lxr.db -i $MYID myfile
lxr_restore -x ~/lxr.chunks -d ~/lxr.db -i $MYID -o /tmp myfile && cmp myfile /tmp/myfile
```

This demo keeps chunks and database inside the container, they are gone when it exits; for real use mount volumes for both (see [docker/README.md](docker/README.md)).
The chunks (`-x`) can be stored anywhere; the database (`-d`) contains the keys and must be kept safe, see [Operations](doc/09_Operations.md).

## What it protects against

_eLyKseeR_ keeps backups confidential and tamper-evident on storage you do not trust (USB sticks, S3/MinIO, other people's disks):

- file data is split into blocks, collected in assemblies of 16–256 chunks of 256 KiB, and each assembly is encrypted with AES-256-GCM under its own random key; the assembly id is authenticated, so chunks cannot be swapped or modified unnoticed ([format](doc/08_Format.md), [keys and nonces](doc/07_Keys_and_Nonces.md))
- chunk file names are hashes; they reveal neither file names nor sizes, only the total amount of data
- blocks are deduplicated, so unchanged data is not stored twice

It does **not** protect:

- the meta data database (`-d`): it holds the encryption keys in plain text and must stay on trusted storage; whoever has it and the chunks can read the backup, whoever loses it cannot
- availability: deleted or lost chunks make the affected files unrecoverable; distribute chunks to several sinks with `lxr_distribute`
- the machine running the backup, and data before it is encrypted

[License](LICENSE): [GNU General Public License v3 or later](https://www.gnu.org/licenses)

## Contributing

This is an open source project and open for your contributions! Before submitting your first pull request, please review our [Contributor License Agreement](CLA.md) (CLA). The CLA allows us to maintain _eLyKseeR_ under a dual-licensing model (GPLv3 for open source use, with commercial licenses available for proprietary applications).

We very much appreciate user experience testimonials, bug reports, feature requests, documentation improvements, and code contributions. See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

## Project setup and preparations

0.) if not yet done: `opam init -a --bare`

1.) create a compiler switch: `opam switch create 5.2.1`

### Rocq version used

(the opam package is still called `coq`)

`opam pin add coq 9.0.0`

### more packages

`opam repo add coq-released https://coq.inria.fr/opam/released`

see [docker/Dockerfile.base](docker/Dockerfile.base) and [docker/Dockerfile](docker/Dockerfile) which other packages are installed.

## Code generation

0. create _Coq_'s Makefile
   `coq_makefile -f _CoqProject -o Makefile`

1. run proofs in _Coq_ and extract _ML_ code:
   `make`

2. build library:
   `dune build`

3. build and run executable (shows help):
   `dune exec lxr_backup -- -h`

## Dependencies

- [elykseer-crypto](https://github.com/eLyKseeR/elykseer-crypto)
  provides cryptographic primitives

- [mlcpp_filesystem](https://github.com/CodiePP/ml-cpp-filesystem)
  OCaml integration with C++ standard library `<filesystem>`

- [mlcpp_cstdio](https://github.com/CodiePP/ml-cpp-cstdio)
  OCaml integration with C++ standard library `<cstdio>`

- [mlcpp_chrono](https://github.com/CodiePP/ml-cpp-chrono)
  OCaml integration with C++ standard library `<chrono>`

## Docker image

The images are available for Linux/amd64 and Linux/arm64 on [Docker hub](https://hub.docker.com/r/codieplusplus/elykseer-ml).
Docker picks the matching architecture automatically.

download it from Docker Hub:

```sh
docker pull codieplusplus/elykseer-ml:latest
```

run the image:

```sh
docker run --rm -it codieplusplus/elykseer-ml:latest
```

(one can also attach a local Visual Code editor to this container; install extensions "VsCoq" and "OCaml Platform" for source code highlighting)

### building the images

The build is split into two images, all tags live in `codieplusplus/elykseer-ml`:

| image | Dockerfile | tag | contents | rebuild when |
| ----- | ---------- | --- | -------- | ------------ |
| base  | [docker/Dockerfile.base](docker/Dockerfile.base) | `base_${BASE_VERSION}` | Debian, OCaml, Rocq, opam packages (irmin, lwt, ezcurl, ...) | upgrading OCaml, Rocq or opam packages |
| main  | [docker/Dockerfile](docker/Dockerfile) | `${VERSION}`, `latest` | C++ dependencies, elykseer-crypto, proofs, extracted code, `lxr_*` binaries | every release |

Building the base image takes a long time; it only needs to be done once, and again on upgrades.
All commands are run from within `docker/` and need `docker buildx`.

Both images in one step, without pushing the base image first ([docker/docker-bake.hcl](docker/docker-bake.hcl)); add `--push` to publish:

```sh
cd docker/
BASE_VERSION=2 VERSION=v0.10.1 docker buildx bake --allow=fs.read=../.git
```

The main image contains a shallow clone of the committed `HEAD` only (no other branches, reflogs, or remotes of the build machine).

#### 1. base image

OCaml and Rocq versions are build arguments (defaults: `OCAML_VERSION=5.2.1`, `COQ_VERSION=9.0.0`).

in one step, both architectures:

```sh
cd docker/
BASE_VERSION=2  # increment on every upgrade

docker buildx build -f Dockerfile.base --platform linux/amd64,linux/arm64 \
  --build-arg OCAML_VERSION=5.2.1 --build-arg COQ_VERSION=9.0.0 \
  -t codieplusplus/elykseer-ml:base_${BASE_VERSION} \
  --push .
```

or, in two separate steps (each on a native builder, which avoids slow emulation), then combined:

```sh
cd docker/
BASE_VERSION=2

# amd64 leg
docker buildx build -f Dockerfile.base --platform linux/amd64 \
  -t codieplusplus/elykseer-ml:base_amd64_${BASE_VERSION} \
  --push .

# arm64 leg (e.g. native Apple Silicon)
docker buildx build -f Dockerfile.base --platform linux/arm64 \
  -t codieplusplus/elykseer-ml:base_arm64_${BASE_VERSION} \
  --push .

docker buildx imagetools create \
  -t codieplusplus/elykseer-ml:base_${BASE_VERSION} \
  codieplusplus/elykseer-ml:base_amd64_${BASE_VERSION} \
  codieplusplus/elykseer-ml:base_arm64_${BASE_VERSION}
```

#### 2. main image

The main image builds from the committed state of this repository: its `.git` is passed in as the named build context `elykseer-ml-git`.
Uncommitted changes are not included.
Select the base image with `--build-arg BASE_VERSION=...`.

in one step, both architectures:

```sh
cd docker/
VERSION="v0.10.1"  # adapt
BASE_VERSION=2

docker buildx build --build-context elykseer-ml-git=../.git \
  --build-arg BASE_VERSION=${BASE_VERSION} \
  --platform linux/amd64,linux/arm64 \
  -t codieplusplus/elykseer-ml:latest \
  -t codieplusplus/elykseer-ml:${VERSION} \
  --push -f Dockerfile .
```

or, in two separate steps, then combined (the legs pull `base_${BASE_VERSION}` from the registry, so push the base image first):

```sh
cd docker/
VERSION="v0.10.1"  # adapt
BASE_VERSION=2

# amd64 leg
docker buildx build --build-context elykseer-ml-git=../.git \
  --build-arg BASE_VERSION=${BASE_VERSION} --platform linux/amd64 \
  -t codieplusplus/elykseer-ml:amd64_${VERSION} \
  --push -f Dockerfile .

# arm64 leg (e.g. native Apple Silicon)
docker buildx build --build-context elykseer-ml-git=../.git \
  --build-arg BASE_VERSION=${BASE_VERSION} --platform linux/arm64 \
  -t codieplusplus/elykseer-ml:arm64_${VERSION} \
  --push -f Dockerfile .

docker buildx imagetools create \
  -t codieplusplus/elykseer-ml:latest \
  -t codieplusplus/elykseer-ml:${VERSION} \
  codieplusplus/elykseer-ml:amd64_${VERSION} \
  codieplusplus/elykseer-ml:arm64_${VERSION}
```

check that both architectures are present:

```sh
docker buildx imagetools inspect codieplusplus/elykseer-ml:${VERSION}
```

Each two-platform build needs about 20 GB in the Docker VM. If space is short, run the legs one after the other and clear the build cache in between (`docker buildx prune -af`); every leg is pushed, so nothing is lost.

The CI workflows (`.github/workflows/{ci,it}.yml`, `.forgejo/workflows/{CI,IT}.yaml`) pin the main image by digest; after a release, update them with the digest printed by `imagetools inspect` (line `Digest:`).

## Executables

<details>
<summary>Backup</summary>

### lxr_backup - backup files indicated on the command line to LXR

```
lxr_backup -x chunkpath -d dbpath [-v] [-y] [-n nchunks] [-i myid] [-D directory [-R]] [<file1> ...]
  -v verbose output
  -y dry run
  -x sets output path for encrypted chunks
  -d sets database path
  -n sets number of chunks (16-256) per assembly
  -i sets own identifier
  -R recursively backup the directory
  -D directory to backup
  -help  Display this list of options
  --help  Display this list of options
```

`-x` and `-d` are required: the database holds the encryption keys and must be kept (see [Operations](doc/09_Operations.md)).
The exit code is 1 if any file could not be backed up or if the meta data is inconsistent, 2 on a usage error.

##### example

This examples assumes that an _irmin_ database exists at path `/data/elykseer.db`.
Create here a file `irmin.yml` with content:

```
root: /data/elykseer.db
store: git
contents: json-value
```

and initialise: `irmin init`

Moreover, the environment variable `$MYID` contains a unique string to distinguish between setups.

```
MYID="424242"
```

backup three files:

`dune exec lxr_backup -- -v -x /data/elykseer.chunks -d /data/elykseer.db -n 16 -i $MYID ./test1M ./test4M ./test8M`

compute file hash:

`FHASH=$(./_build/default/bin/lxr_filehash.exe -f ./test1M)`

get block meta data for this file as CSV output:

```
irmin get ${MYID}/relfiles/${FHASH:4:2}/${FHASH} | jq -r '
  .blocks[] | [.blockaid,.blockapos,.filepos,.blocksize] | @csv' | awk "{print \"${FHASH},\"\$0}"
```

lists:

```
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","0","0","131072"
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","131072","131072","131072"
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","262144","262144","131072"
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","393216","393216","131072"
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","524288","524288","131072"
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","655360","655360","131072"
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","786432","786432","131072"
86b16ef62a8334325e612629cf25b26a77aaa0a59f50024ba9be5b3eb64d90b5,"0fd37df6acbce4a3c99a89161ce7f629aab205cac51cc71126dca0540c4ce437","917504","917504","131072"
```

</details>

<details>
<summary>Restore</summary>

### lxr_restore - restore file(s) from LXR

```
lxr_restore -x chunkpath -d dbpath [-o outpath] [-v] [-n nchunks] [-i myid] <file1> [<file2>] ...
       lxr_restore --verify -x chunkpath -d dbpath [-v] [-n nchunks] [-i myid] [<file1> ...]
  -v verbose output
  -x sets path for encrypted chunks
  -o sets output path for restored files (default: temp directory)
  -d sets database path
  -n sets number of chunks (16-256) per assembly
  -i sets own identifier
  --verify restores into a temporary directory, compares each file with its checksum from the meta data, and removes it; without file names: all files of the identifier
  -help  Display this list of options
  --help  Display this list of options
```

The exit code is 1 if any file could not be restored completely (an incomplete file is removed), 2 on a usage error or if there is no database at `-d`.

`--verify` checks files without keeping them: each file is restored into a temporary directory and compared with its checksum from the meta data; without file names, all files of the identifier are checked.

#### example

```
./_build/default/bin/lxr_restore.exe -v -x /data/elykseer.chunks -d /data/elykseer.db -o /tmp/ -i $MYID test4M test8M
```

outputs:

```
  restoring 8388607 bytes in file 'test8M' from 64 blocks
+✅ 'test8M'    restored with 8388607 bytes in total
  restoring 4194304 bytes in file 'test4M' from 32 blocks
+✅ 'test4M'    restored with 4194304 bytes in total
  restored 2 files with 12582911 bytes in total
```

File names are restored relative to the output directory: an absolute name like `/home/me/test4M` is restored to `/tmp/home/me/test4M`; names containing `..` are refused.

The files were extracted to `/tmp/` and can be compared with: `md5sum /tmp/test4M test4M /tmp/test8M test8M`

```
83b1a2506a5d1a50dd645ac59c35d147  /tmp/test4M
83b1a2506a5d1a50dd645ac59c35d147  test4M
e4379d58904294ab7ab6431191cd9801  /tmp/test8M
e4379d58904294ab7ab6431191cd9801  test8M
```

</details>

<details>
<summary>Encryption keys</summary>

### lxr_relkeys - export keys from meta data

```
lxr_relkeys -d dbpath [-v] [-x] [-i myid] <file1> [<file2>] ...
  -v verbose output
  -x XML output
  -d sets database path
  -i sets own identifier
  -help  Display this list of options
  --help  Display this list of options
```

#### example

This examples assumes that an _irmin_ database exists at path `/data/elykseer.db`.
And, the environment variable `$MYID` is set to the same value as in the backup.

extract keys to XML:

`./_build/default/bin/lxr_relkeys.exe -v -x -d /data/elykseer.db -i $MYID test1G > k.xml`

verify data against XML schema:

`xmllint --schema schema/keys.xsd k.xml --noout`

</details>

<details>
<summary>File information and blocks</summary>

### lxr_relfiles - export file meta data

```
lxr_relfiles -d dbpath [-v] [-x] [-i myid] <file1> [<file2>] ...
  -v verbose output
  -x XML output
  -d sets database path
  -i sets own identifier
  -help  Display this list of options
  --help  Display this list of options
```

#### example

This examples assumes that an _irmin_ database exists at path `/data/elykseer.db`.
And, the environment variable `$MYID` is set to the same value as in the backup.

extract keys to XML:

`./_build/default/bin/lxr_relfiles.exe -v -x -d /data/elykseer.db -i $MYID test1G > b.xml`

verify data against XML schema:

`xmllint --schema schema/fileinformation.xsd b.xml --noout && echo OK || echo failed`

</details>
