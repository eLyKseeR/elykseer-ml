# Changelog

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