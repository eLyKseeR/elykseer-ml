# eLyKseeR

This project is about developing the cryptographic data archive _eLyKseeR_ using formal methods and implementing it in the functional language _Rocq_ (former _Coq_).

Copyright (c) 2026 Alexander Diemand

[License](LICENSE): [GNU General Public License v3 or later](https://www.gnu.org/licenses)

## Contributing

This is an open source project and open for your contributions! Before submitting your first pull request, please review our [Contributor License Agreement](CLA.md) (CLA). The CLA allows us to maintain _eLyKseeR_ under a dual-licensing model (GPLv3 for open source use, with commercial licenses available for proprietary applications).

We very much appreciate user experience testimonials, bug reports, feature requests, documentation improvements, and code contributions. See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

## Project setup and preparations

0.) if not yet done: `opam init -a --bare`

1.) create a compiler switch: `opam switch create 5.2.1`

### Coq version used

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
  OCaml integration with C++ standard library &gt;filesystem&lt; module

- [mlcpp_cstdio](https://github.com/CodiePP/ml-cpp-cstdio)
  OCaml integration with C++ standard library &gt;cstdio&lt; module

- [mlcpp_chrono](https://github.com/CodiePP/ml-cpp-chrono)
  OCaml integration with C++ standard library &gt;chrono&lt; module

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

#### 1. base image

OCaml and Rocq versions are build arguments (defaults: `OCAML_VERSION=5.2.1`, `COQ_VERSION=9.0.0`).

in one step, both architectures:

```sh
cd docker/
BASE_VERSION=1  # increment on every upgrade

docker buildx build -f Dockerfile.base --platform linux/amd64,linux/arm64 \
  --build-arg OCAML_VERSION=5.2.1 --build-arg COQ_VERSION=9.0.0 \
  -t codieplusplus/elykseer-ml:base_${BASE_VERSION} \
  --push .
```

or, in two separate steps (each on a native builder, which avoids slow emulation), then combined:

```sh
cd docker/
BASE_VERSION=1

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
VERSION="v0.9.16"  # adapt
BASE_VERSION=1

docker buildx build --build-context elykseer-ml-git=../.git \
  --build-arg BASE_VERSION=${BASE_VERSION} \
  --platform linux/amd64,linux/arm64 \
  -t codieplusplus/elykseer-ml:latest \
  -t codieplusplus/elykseer-ml:${VERSION} \
  --push -f Dockerfile .
```

or, in two separate steps, then combined:

```sh
cd docker/
VERSION="v0.9.16"  # adapt
BASE_VERSION=1

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

## Executables

<details>
<summary>Backup</summary>

### lxr_backup - backup files indicated on the command line to LXR

```
lxr_backup: vyxdnji
  -v verbose output
  -y dry run
  -x sets output path for encrypted chunks
  -d sets database path
  -n sets number of chunks (16-256) per assembly
  -j sets number of parallel processes
  -i sets own identifier
  -help  Display this list of options
  --help  Display this list of options
```

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
lxr_restore: vxodnji
  -v verbose output
  -x sets path for encrypted chunks
  -o sets output path for restored files
  -d sets database path
  -n sets number of chunks (16-256) per assembly
  -j sets number of parallel processes
  -i sets own identifier
  -help  Display this list of options
  --help  Display this list of options
```

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
<summary>Verify</summary>

### lxr_compare - compare file(s) against backuped blocks in LXR

```
lxr_compare: vdi
  -v verbose output
  -d sets database path
  -i sets own identifier
  -help  Display this list of options
  --help  Display this list of options
```

#### example

```
./_build/default/bin/lxr_compare.exe -v -d /data/elykseer.db -i $MYID ./test4M
```

outputs:

```
comparing file ./test4M against meta data
 +✅ block 1@0=131072
 +✅ block 2@131072=131072
 +✅ block 3@262144=131072
 +✅ block 4@393216=131072
 +✅ block 5@524288=131072
 +✅ block 6@655360=131072
 +✅ block 7@786432=131072
 +✅ block 8@917504=131072
 +✅ block 9@1048576=131072
 +✅ block 10@1179648=131072
 +✅ block 11@1310720=131072
 +✅ block 12@1441792=131072
 +✅ block 13@1572864=131072
 +✅ block 14@1703936=131072
 +✅ block 15@1835008=131072
 +✅ block 16@1966080=131072
 +✅ block 17@2097152=131072
 +✅ block 18@2228224=131072
 +✅ block 19@2359296=131072
 +✅ block 20@2490368=131072
 +✅ block 21@2621440=131072
 +✅ block 22@2752512=131072
 +✅ block 23@2883584=131072
 +✅ block 24@3014656=131072
 +✅ block 25@3145728=131072
 +✅ block 26@3276800=131072
 +✅ block 27@3407872=131072
 +✅ block 28@3538944=131072
 +✅ block 29@3670016=131072
 +✅ block 30@3801088=131072
 +✅ block 31@3932160=131072
 +✅ block 32@4063232=131072
comparison of 1 file with 1 equal
```

</details>

<details>
<summary>Encryption keys</summary>

### lxr_relkeys - export keys from meta data

```
lxr_relkeys [-v] [-i myid] [-d dbpath] <file1> [<file2>] ...
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
lxr_relfiles [-v] [-i myid] [-n nchunks] [-d dbpath] <file1> [<file2>] ...
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
