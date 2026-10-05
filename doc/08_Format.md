[Content](00_Content.md)

# Archive format (version 0.10)

This describes what is written to disk: chunks with encrypted data, and the meta data database. Archives of version 0.10 are not compatible with 0.9.x (AES-256-CBC), see [Upgrading](09_Operations.md#upgrading-from-09x).

## Terms

| term | size | |
|---|---|---|
| block | ≤ 32 KiB | a piece of a file, the unit of deduplication |
| chunk | 256 × 1024 = 262144 bytes | a file on disk (or in a sink) |
| assembly | n × 262144 bytes, n = 16–256 (`-n`) | n chunks, encrypted as a whole under one key |

## Data layout in an assembly

Blocks are appended to an assembly at the *logical* position `apos`. The bytes are striped over the chunks (`elykseer-base/assembly.cxx`, `idx2apos`), see [Storage](03_Storage.md):

```
logical index i  ->  chunk (i mod n), offset (i div n)  ->  physical position (i mod n) * 262144 + (i div n)
```

Adjacent bytes of a file therefore end up in different chunks.

Logical layout:

| logical range | content |
|---|---|
| `[0, h)` | random header, h = min(n − 16, 128) bytes (0 for n = 16) |
| `[h, apos)` | blocks, in the order they were backed up |
| up to `n·262144 − 16·n` | capacity for data |
| last `16·n` bytes | reserved: their last row holds the GCM tag (see below) |

Physical layout:

| physical range | content |
|---|---|
| `[0, n·262144 − 16)` | AES-256-GCM ciphertext |
| last 16 bytes (end of the last chunk) | GCM authentication tag |

Note: up to version 0.10.0 only 16 logical bytes were reserved. Data in the last rows of an assembly could then share physical positions with the tag and was overwritten on encryption; such blocks fail their checksum on restore. Fixed in 0.10.1.

## Encryption

- AES-256-GCM over the whole assembly, one key information record per assembly ([keys and nonces](07_Keys_and_Nonces.md)):
  - key: `pkey`, 256 bit, hex encoded
  - nonce: the first 96 bits (24 hex characters) of `ivec` (128 bit, hex encoded)
  - additional authenticated data (AAD): the assembly id `aid` (hex string)
- decryption fails on a wrong key, nonce, or aid, and on any modified or missing byte.

## Identifiers

| identifier | definition |
|---|---|
| `aid` | SHA3-256 (hex) of `my_id` and 256 random bits (`rnd256` in `theories/MakeML.v`; before 0.10.1: time, host name, `my_id` and only 32 random bits) |
| chunk id | SHA3-256 (hex) of `my_id ^ cid ^ aid`, for cid = 1..n |
| chunk path | `<chunkpath>/<c63c64>/<c61c62>/<chunk id>.lxr`, the subdirectories are the last four hex characters of the chunk id |
| file hash `fhash` | SHA3-256 (hex) of `fname ^ my_id` |

Chunk number cid holds the physical bytes `[(cid − 1)·262144, cid·262144)` of the encrypted assembly. A chunk file has exactly 262144 bytes.

## Meta data database

An [irmin](https://irmin.org) git store (`-d`), contents are JSON values:

| path | content |
|---|---|
| `<my_id>/relkeys/<aid[4..5]>/<aid>` | `{ "version": {...}, "keys": { "ivec", "pkey", "localnchunks" } }` |
| `<my_id>/relfiles/<fhash[4..5]>/<fhash>` | `{ "version": {...}, "fileinformation": {...}, "blocks": [ { "blockid", "bchecksum", "blocksize", "filepos", "blockaid", "blockapos" }, ... ] }` |

- keys are stored in **plain text**; the database must be protected like the data itself ([Operations](09_Operations.md))
- `blockaid` and `blockapos` locate a block in its assembly (logical position); `bchecksum` is its SHA3-256, verified on restore
- export as CSV or XML with `lxr_relkeys` and `lxr_relfiles`; XML schemas in `schema/`
