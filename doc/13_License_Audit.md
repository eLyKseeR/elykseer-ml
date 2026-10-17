[Content](00_Content.md)

# Licence audit: elykseer-ml, elykseer-cpp, elykseer-crypto

Status: **audit, 2026-10-17**. Not legal advice; have the conclusions in §6 confirmed by a
lawyer before selling licences.

**Goal set by the project owner:** licence independence. The code and binaries should be
shareable and usable as the basis of a business, without being tied to a third-party GPL
library.

---

## 0. Summary

* **Your own code is not the problem.** elykseer-ml, elykseer-cpp, elykseer-crypto and
  os-metrics are GPL-3.0, but almost all commits are by Alexander Diemand, and elykseer-ml has a
  CLA that already allows dual licensing (`CLA.md` §4). As copyright holder you can keep GPL-3
  for the public and sell commercial licences at the same time.
* **No third-party GPL library is linked today.** Everything linked is permissive (MIT, BSD,
  ISC, Apache-2.0, Boost, Zlib) or LGPL.
* **Three things need action:**
  1. **gpgme, libassuan and libgpg-error (LGPL-2.1+) are linked statically** into
     `libelykseer-crypto.a`, and through it into every OCaml binary — even though the OCaml side
     never uses GPG. → remove them from the OCaml/C++ archive, or link them dynamically.
  2. **Rocq standard library (LGPL-2.1) code is in the extracted `lxr.ml`** (N/positive
     arithmetic, lists), linked statically. → map those types to OCaml natives during
     extraction, and get a legal opinion on the rest.
  3. **CLA coverage:** elykseer-cpp and elykseer-crypto have no CLA file; elykseer-ml has one
     outside contribution (doc images by `aemchen`, #12) whose CLA status needs checking.
* **For the new plans (docs 10–12):** link only permissive libraries; LGPL only dynamically;
  anything GPL (Samba, GnuPG, …) only as a **separate process**. The affected choices are listed
  in §5.

---

## 1. Own code

| repository | licence | authors (commits, from git history) | CLA |
|---|---|---|---|
| elykseer-ml | GPL-3.0-or-later | A. Diemand 82 · Claude (AI-assisted, on your behalf) 2 · `aemchen` 1 (doc images) | yes, `CLA.md` with dual-licensing clause; `CONTRIBUTORS.md` is empty |
| elykseer-cpp | GPL-3.0 | A. Diemand only (151 + 3) | none |
| elykseer-crypto | GPL-3.0-or-later | A. Diemand only (189 + 5) | none |
| CodiePP/os-metrics | GPL-3.0 | own | – |
| CodiePP/sizebounded | BSD-3-Clause | own | – |
| CodiePP/stream-cpp | BSD-3-Clause | own | – |
| CodiePP/ml-cpp-cstdio, ml-cpp-chrono, ml-cpp-filesystem | Apache-2.0 | own | – |

Note: `git log --author=aemchen` shows that the only outside contribution changes
`doc/img/img1–5.png`. Either get a signed CLA (and add the name to `CONTRIBUTORS.md`) or redraw
the images. No code is affected.

---

## 2. Third-party dependencies — elykseer-crypto

From `ext/Makefile`, `CMakeLists.txt`, `src/cpp/CMakeLists.txt`, `src/ml/lib/dune`.

| dependency | licence | how it is linked | used for | status |
|---|---|---|---|---|
| OpenSSL 3 (default, `USE_OPENSSL=ON`) | Apache-2.0 | static (`libcrypto.a`, `libssl.a`) | AES-GCM, hashes, base64, random | ✅ permissive; keep the NOTICE |
| Crypto++ (alternative, `USE_CRYPTOPP`) | Boost Software License 1.0 | static | same | ✅ |
| **gpgme** | **LGPL-2.1-or-later** | **static, merged into `libelykseer-crypto.a`** (`ar -M` script `addlib …/libgpgme.a`) | `gpg.cpp` (C++ side only) | ⚠️ action |
| **libassuan** | **LGPL-2.1-or-later** | **static, merged** | needed by gpgme | ⚠️ action |
| **libgpg-error** | **LGPL-2.1-or-later** | **static, merged** | needed by gpgme | ⚠️ action |
| GnuPG (`gpg` binary, started by gpgme) | GPL-3 | separate process at run time | – | ✅ separate program; not shipped |
| sizebounded | BSD-3 (own) | header | buffers | ✅ |

**The problem:** the OCaml library `elykseer_crypto` (`src/ml/lib/dune`) has no `gpg` module,
but it links `foreign_archives elykseer-crypto_cpp`, i.e. the merged archive that includes gpgme,
libassuan and libgpg-error. Static linking of LGPL code into a binary distributed under a
proprietary licence is allowed only if the recipient gets everything needed to relink against a
modified version of the LGPL library (object files or source of the rest, LGPL-2.1 §6). That is
impractical for a commercial binary.

**Fix (recommended order):**

1. Build a second archive without `gpg.cpp` and without the three GnuPG libraries for the OCaml
   binding (and for elykseer-cpp if it does not use GPG). The ML side loses nothing.
2. Where GPG is needed (C++ key export?), link gpgme **dynamically** (`libgpgme.so`): this
   meets the LGPL without any special distribution requirements.
3. Long term, if GPG integration is a product feature, call the `gpg` command line as a separate
   process instead of linking gpgme at all.

---

## 3. Third-party dependencies — elykseer-cpp

| dependency | licence | linking | status |
|---|---|---|---|
| elykseer-crypto | own (see §2) | static | ⚠️ inherits the gpgme issue |
| Boost (system, date_time, contract) | Boost Software License 1.0 | shared (`Boost_USE_STATIC_LIBS OFF`) | ✅ |
| zlib | Zlib | system | ✅ |
| HowardHinnant/date | MIT | header | ✅ |
| rapidcheck (tests only) | BSD-2-Clause | test binaries | ✅ |
| sizebounded, stream-cpp | BSD-3 (own) | header | ✅ |
| os-metrics | GPL-3 (own) | linked | ✅ own code; relicense or dual-license it along with the rest |

---

## 4. Third-party dependencies — elykseer-ml

Direct dependencies from the `dune` files and `docker/Dockerfile.base`. Transitive OCaml
dependencies, especially of irmin, must be checked on a build machine (command in §7).

| dependency | licence | linking | status |
|---|---|---|---|
| OCaml runtime and stdlib, `unix` | LGPL-2.1 **with static-linking exception** | static | ✅ the exception covers proprietary binaries |
| Rocq/Coq **stdlib, as extracted code** in `lxr.ml` | **LGPL-2.1-only**, no linking exception | static (part of the program) | ⚠️ see below |
| Rocq/Coq compiler | LGPL-2.1 | build tool only | ✅ |
| coq-record-update | MIT | build time | ✅ |
| dune, lwt, lwt_ppx | MIT | static | ✅ |
| yojson | BSD-3 | static | ✅ |
| irmin, irmin-git, irmin.unix | ISC | static | ✅ direct; **transitive deps to verify** |
| ocaml-git and the irmin stack (cohttp, conduit, tls, x509, mirage-crypto, digestif, …) | expected MIT/ISC/BSD | static | 🔎 verify |
| zarith (if pulled in, e.g. through mirage-crypto-pk) | LGPL-2 **with OCaml linking exception** | static | ✅ if confirmed |
| **GMP** (via zarith; `libgmp-dev` in the images) | LGPL-3 or GPL-2 (dual) | **must stay dynamic** | 🔎 check `ldd` of the binaries; never link statically |
| ocurl | MIT | static | ✅ |
| ezcurl, ezcurl-lwt | MIT | static | ✅ |
| libcurl | curl licence (MIT-style) | dynamic | ✅ |
| **GnuTLS** (via `libcurl4-gnutls-dev`) | LGPL-2.1-or-later | dynamic | ✅ dynamic is fine; switching to `libcurl4-openssl-dev` removes the question |
| libffi | MIT | dynamic | ✅ |
| elykseer_crypto | own | static | ⚠️ gpgme issue, §2 |
| mlcpp_cstdio, mlcpp_chrono, mlcpp_filesystem | Apache-2.0 (own) | static | ✅ |
| alcotest, alcotest-lwt, qcheck, qcheck-alcotest | ISC / BSD-2 | tests only | ✅ |
| bisect_ppx, ppx_optcomp | MIT | instrumentation / build | ✅ |

**Rocq stdlib in the extracted code.** `theories/MakeML.v` uses `ExtrOcamlBasic` and
`ExtrOcamlNativeString`, but the numeric types (`N`, `positive`) and many list functions are
extracted *from their Rocq definitions*, so `lxr.ml` contains code translated from the
LGPL-2.1-only Rocq standard library (56 places refer to `BinNat`/`Pos.`/`N.` functions). The
Rocq licence has no special clause for extracted programs. Whether extracted code is a
"work based on the library" is not settled. Steps that reduce the question to nearly nothing:

1. Extract `N` and `positive` to OCaml `int` (or zarith) with `Extract Inductive` /
   `Extract Constant` for the arithmetic used (`ExtrOcamlNatInt`-style). This also speeds up the
   code.
2. Map the list and string helpers used to OCaml `List`/`String` functions.
3. Check what remains in `lxr.ml` that comes from `Stdlib` (grep the module names), and have a
   lawyer look at that remainder. The usual industry practice (CompCert, Rocq-based products)
   is to treat extracted programs as the author's own work, but that is an opinion, not a
   guarantee.

---

## 5. Consequences for the plans in docs 10–12

Rule: **permissive = link freely; LGPL = dynamic link only; GPL/AGPL = separate process only.**

| plan | item | licence | decision |
|---|---|---|---|
| [10 GPU](10_GPU_AssemblyCache.md) | CUDA runtime / driver API | NVIDIA EULA (redistributable runtime parts) | ✅ dynamic, as usual |
| | HIP/ROCm | MIT | ✅ |
| | metal-cpp | Apache-2.0 | ✅ |
| | Vulkan loader, headers | Apache-2.0 | ✅ |
| [11 Sinks](11_Sinks_and_Sources.md) | libcurl (S3, WebDAV, SFTP, IMAP) | curl (MIT-style); libssh2 BSD-3 | ✅ — build libcurl against OpenSSL rather than GnuTLS |
| | **libsmb2** (SMB driver) | **LGPL-2.1** | ⚠️ dynamic only, or use a mounted share with the FS driver |
| | **libnfs** (NFS driver) | **LGPL-2.1** (parts BSD) | ⚠️ dynamic only, or use a mount |
| | rclone (exec driver) | MIT | ✅ separate process anyway |
| [12 Serving](12_Serving_the_AssemblyCache.md) | in-house NFSv3 / WebDAV / index / cache service | own | ✅ the recommended core |
| | libfuse 3 | LGPL-2.1 | ⚠️ dynamic only (normal for libfuse), own stubs |
| | **ocamlfuse** | GPL | ❌ do not use |
| | ocaml-9p | ISC | ✅ |
| | cohttp, h2, Dream, ocaml-grpc | ISC / BSD / MIT | ✅ |
| | go-nfs, nfsserve | Apache-2.0 / BSD-3 | ✅ if a second language is acceptable |
| | **nfs-ganesha** | **LGPL-3** | ⚠️ an FSAL plug-in is linked into Ganesha: keep it a separate open component, or avoid |
| | **Samba** | **GPL-3** | ✅ only as a separate process re-exporting a mount; do not ship it inside a product image without GPL compliance for Samba itself |
| | OpenSSH | BSD-style | ✅ |
| | macFUSE / FUSE-T | proprietary | ❌ avoid; use the NFS/WebDAV mounts built into macOS |
| | WinFsp | GPL-3 with FLOSS exception | ❌ for a proprietary product (the exception covers FLOSS only); commercial licence available from its author, or use the built-in NFS/WebDAV clients |
| | csi-driver-nfs, lib-volume-populator, CSI spec | Apache-2.0 | ✅ |

---

## 6. Licensing options for the business

Because you hold (nearly) all copyrights, these are open to you:

| model | how | needs |
|---|---|---|
| **dual licence** (GPL-3 public + commercial) | the current setup; customers who don't want GPL obligations buy a licence | CLA for every contributor (exists in ml; add to cpp/crypto); all *dependencies* permissive or dynamically linked LGPL (this audit) |
| **open core** | core under a permissive or GPL licence, enterprise features (serving, CSI, GPU) proprietary | clean module boundaries; same dependency rules |
| **service (SaaS)** | run the AssemblyCache service for customers | GPL-3 imposes nothing on hosted use (AGPL would); dependency rules still apply to anything you *distribute* (agents, CSI images) |
| **move to a permissive licence** (Apache-2.0 / MPL-2.0) | relicense the own code | your decision as copyright holder; outside contributions must agree (CLA) |

Container images are distributions too: the Docker images (`docker/`) ship Debian packages,
including GPL ones (bash, coreutils, GnuPG if installed). That is normal and allowed, but the
images must offer the corresponding source for those packages. A minimal runtime image
(distroless or a static base with only the required shared libraries) reduces that burden.

---

## 7. Actions

| # | action | where | effort |
|---|---|---|---|
| 1 | Archive without gpgme/libassuan/libgpg-error for the OCaml binding (and C++ users that do not need GPG); dynamic gpgme where it is needed | elykseer-crypto `src/cpp/CMakeLists.txt`, `src/ml/lib/dune` | 0.5–1 d |
| 2 | Extract `N`/`positive`/lists to OCaml natives; inventory the remaining Stdlib-derived code in `lxr.ml` | elykseer-ml `theories/MakeML.v` | 2–4 d (with test runs) |
| 3 | Add `CLA.md` + `CONTRIBUTORS.md` to elykseer-cpp and elykseer-crypto; settle the CLA of `aemchen` (or redraw `doc/img/*.png`) | all repos | 0.5 d |
| 4 | Switch libcurl to the OpenSSL build (`libcurl4-openssl-dev`) | `docker/Dockerfile*` | 0.5 d |
| 5 | Inventory the transitive OCaml deps on the build image and record them: `opam list --required-by elykseer --recursive --columns name,version,license:` (after pinning the package locally) | CI | 0.5 d |
| 6 | Check `ldd` of every shipped binary: GMP, GnuTLS, libfuse (later) must be dynamic | CI | 0.5 d |
| 7 | Add a licence check to CI (fail on new GPL/AGPL/LGPL-static deps), and generate a `THIRD_PARTY_NOTICES` file (OpenSSL, Boost, BSD/MIT notices) for releases | CI, release | 1–2 d |
| 8 | Record the dependency rule (§5) in `CONTRIBUTING.md` | all repos | 1 h |
| 9 | Decide the business model (§6) and have a lawyer review this audit, especially §4 (Rocq extraction) | – | – |

Commands used to collect the data for this audit (reproducible):

```sh
git -C <repo> shortlog -sne HEAD                     # authors
grep -rn "addlib\|STATIC IMPORTED" elykseer-crypto/src/cpp/CMakeLists.txt elykseer-crypto/CMakeLists.txt
grep -rhoE "\(libraries[^)]*\)" --include=dune elykseer-ml
head -3 <dependency>/LICENSE
```
