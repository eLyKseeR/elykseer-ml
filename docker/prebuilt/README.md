# Prebuilt libraries

`dot.local-<arch>.tgz` unpack to `~/.local` in the build stage of `../Dockerfile` and provide static C/C++ libraries needed to build elykseer-crypto. They are only used at build time, they are not part of the runtime image.

| file | arch | content |
|---|---|---|
| `dot.local-x86_64.tgz` | amd64 | Boost 1.83.0 (static, all libraries except python/mpi/graph_parallel), zlib 1.3 |
| `dot.local-aarch64.tgz` | arm64 | same |

Added in commit 31fc38f (2023-12-30) to shorten image builds.

## Integrity

`SHA256SUMS` records the checksums of the committed tarballs. `../Dockerfile` verifies the tarball with `sha256sum -c` before extracting it, the build fails on a mismatch. Check locally:

```sh
sha256sum -c SHA256SUMS
```

## Sources

| component | version | upstream source | SHA256 of source archive |
|---|---|---|---|
| Boost | 1.83.0 | https://archives.boost.io/release/1.83.0/source/boost_1_83_0.tar.bz2 | `6478edfe2f3305127cffe8caf73ea0176c53769f4bf1585be237eb30798c3b8e` |
| zlib | 1.3 | https://zlib.net/fossils/zlib-1.3.tar.gz | `ff0ba4c292013dbc27530b3a81e1f9a813cd39de01ca5e0f8bf355702efa593e` |

Licenses: Boost Software License 1.0, zlib license; both are GPL-compatible.

## Rebuild

`build-dot-local.sh` downloads both sources, checks the checksums above, builds them into `~/.local`, and packs the tarball. Run it in the base image for each architecture, replace the tarball, and update `SHA256SUMS`:

```sh
bash build-dot-local.sh
sha256sum dot.local-*.tgz > SHA256SUMS
```

The build is not bit-for-bit reproducible (timestamps, compiler version), so a rebuilt tarball has a different checksum than the committed one.
