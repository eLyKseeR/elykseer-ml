# Changelog

## [0.9.12] - 2024-05-23

### Added

- tracing messages to log out
- fixing a bug ml-cpp-filesystem that prevented restoring files in subdirectories
- binary-only Docker image


## [0.9.11] - 2024-05-08

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