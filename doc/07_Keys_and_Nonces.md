[Content](00_Content.md)

# Encryption keys and GCM nonces

Since version 0.10.0 assemblies are encrypted with AES-256-GCM. GCM breaks down completely if the same (key, nonce) pair is ever used to encrypt two different messages: the XOR of the two plaintexts leaks and the authentication key can be recovered, which allows forgeries. This document explains why eLyKseeR never reuses a (key, nonce) pair.

## Key information

Every assembly has its own key information record (`keyinformation` in `theories/Assembly.v`):

| field | size | content |
|---|---|---|
| `pkey` | 256 bit (64 hex chars) | AES-256 key |
| `ivec` | 128 bit (32 hex chars) | initialisation vector; the GCM nonce is its first 96 bit (24 hex chars) |
| `localnchunks` | | number of chunks of the assembly |

The record is stored in the key store (`Relkeys`), indexed by the assembly id (aid).

## One key, one encryption

1. Keys are only generated in `EnvironmentWritable.finalise_assembly` (`theories/Environment.v`), which creates a fresh record for every assembly it finalises:
   - `pkey := cpp_mk_key256 tt`
   - `ivec := cpp_mk_key128 tt`

   both extracted to `Elykseer_crypto.Key256.mk` / `Key128.mk` (`theories/MakeML.v`), i.e. drawn from the random number generator of elykseer-crypto.
2. `Assembly.encrypt` is called in exactly one place, in `finalise_assembly`, with the record just created. An assembly is encrypted once and afterwards only decrypted (`EnvironmentReadable.restore_assembly`); a restored assembly is never re-encrypted.
3. Consequently every key encrypts exactly one message. Even if two nonces happened to be equal, they would belong to different keys, which is safe for GCM. A collision of two random 256-bit keys is negligible (birthday bound ≈ 2<sup>128</sup> assemblies).

The nonce is therefore not required to be unique on its own; it is random as defence in depth.

## Authenticated data and tag

- The aid is passed as additional authenticated data (AAD). Decrypting chunks under a different aid fails, so assemblies cannot be swapped.
- The 16-byte authentication tag occupies the last 16 bytes of the assembly (`Cstdio.tag_len`), the encrypted data stays the same size as the plain assembly.
- Decryption fails explicitly on a wrong key, wrong aid, or modified data.

## What would break the argument

Any change that encrypts with a stored key record again, e.g.
- re-encrypting an assembly after restoring it (append to an existing assembly),
- deriving keys deterministically from the aid or `my_id`,
- reusing one key record for several assemblies,

must also generate a new key (or at least a new nonce under a key that is tracked to never repeat).

## Tests

`test/testAssembly.ml`:
- `key and nonce uniqueness`: 1000 generated key records have pairwise different keys and nonces
- `finalise uses fresh keys`: two finalised assemblies differ in aid, key, and nonce
- `gcm wrong key`, `gcm wrong aid`, `gcm tampered`: decryption fails
