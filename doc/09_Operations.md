[Content](00_Content.md)

# Operations: what to keep, how to verify, what can go wrong

## What a backup consists of

| item | option | if lost |
|---|---|---|
| meta data database (irmin git store) | `-d` | **nothing can be restored**: it holds the encryption keys and the file → block → assembly mapping |
| chunks | `-x` (and sinks of `lxr_distribute`) | files with blocks in a lost assembly cannot be restored |
| identifier | `-i` | meta data and chunks cannot be found (all identifiers depend on it) |
| number of chunks per assembly | `-n` | assemblies cannot be read; use the same value for backup, restore and distribute |
| file names as given to `lxr_backup` | | restore is by name; list them with `lxr_relfiles` |

Record `-i` and `-n` next to the database, they are not secret.

## Protecting the database

The keys are stored in plain text (see [Format](08_Format.md#meta-data-database)). Treat the database like the data itself:

- keep it on trusted storage, readable only by the backup user: `chmod -R go-rwx <dbpath>`
- copy it off the machine after every backup run, encrypted, and never next to the chunks; it is a git repository (in `<dbpath>/.git`), so either of these works while no backup is running:
  ```sh
  tar czf - -C "$(dirname "$DB")" "$(basename "$DB")" | age -r "$RECIPIENT" > lxr-db-$(date +%F).tgz.age
  git -C "$DB" bundle create /secure/lxr-db-$(date +%F).bundle --all   # absolute path: -C changes the directory
  ```
- keep several generations: a damaged database copy is only noticed on restore

## Restore drill

Run it regularly (e.g. monthly) and after every upgrade, on a different machine if possible, from the off-site copies only:

```sh
MYID=...; NCHUNKS=16
mkdir -p drill/chunks drill/out
# 1. database from the off-site copy (or unpack the tar archive)
git clone --no-checkout /secure/lxr-db-YYYY-MM-DD.bundle drill/db
# 2. chunks from the sinks, for each assembly id (see lxr_relfiles output, column "aid")
lxr_distribute -d GET -a "$AID" -n $NCHUNKS -i $MYID -x drill/chunks -c sinks.json 1 1
# 3. verify all files against their checksums
lxr_restore --verify -x drill/chunks -d drill/db -n $NCHUNKS -i $MYID
# 4. restore a sample of files and compare with the originals
lxr_restore -x drill/chunks -d drill/db -n $NCHUNKS -i $MYID -o drill/out file1 file2
cmp file1 drill/out/file1 && cmp file2 drill/out/file2
```

A drill passes if all commands exit with 0 and the files compare equal.

## Exit codes

| tool | 0 | 1 | 2 |
|---|---|---|---|
| `lxr_backup` | all files backed up | a file could not be read or backed up, or a block refers to an assembly without key | usage error |
| `lxr_restore` | all files restored (or verified) completely | a file has no meta data, already exists, escapes `-o`, could not be restored completely (it is removed), or fails `--verify` | usage error, no database |
| `lxr_distribute` | all chunks copied or already present | a chunk could not be copied | usage error, invalid sink configuration |

## Failure modes

| situation | what you see | remedy |
|---|---|---|
| chunk missing | restore: `failed to restore file ...: restored 0 of N bytes`, exit 1 | fetch the chunks of the assembly from a sink (`lxr_distribute -d GET`), restore again |
| chunk corrupted or modified | same: GCM authentication fails for the whole assembly | replace the chunk from another sink |
| wrong `-n` | same | use the value from the backup |
| wrong `-i` | `no meta data found`, exit 1 | use the identifier from the backup |
| database missing or wrong path | `no meta data database found at ...`, exit 2 | restore the database from a copy |
| database lost, no copy | — | none; the archive cannot be decrypted |
| file unreadable during backup | `cannot backup file ...`, exit 1; the other files are backed up | fix permissions and run the backup again (unchanged files are deduplicated) |
| backup interrupted | meta data is only written at the end of a run, so the files of that run are not in the database; chunks already written are orphaned | run the backup again |
| encryption or chunk output failed | `no key for assembly ...`, exit 1 | check free space and permissions of `-x`, back up the reported files again |
| archive written by 0.9.x | decryption fails | see [upgrading from 0.9.x](#upgrading-from-09x) |

## Upgrading from 0.9.x

Version 0.10 encrypts with AES-256-GCM instead of AES-256-CBC; there is no converter, 0.9.x archives cannot be read by 0.10.

1. keep the old tools: Docker image `codieplusplus/elykseer-ml:v0.9.16`
2. restore all files from the old archive with the old tools into a staging directory, verify them
3. back up the staging directory with 0.10 into a **new database and a new chunk directory** (or at least a new `-i`): with the old database, deduplication would find the unchanged files in the meta data and keep referring to the old CBC assemblies
4. run a restore drill against the new archive
5. only then retire the old archive

## Upgrading from 0.10.0

0.10.0 could overwrite up to 15 bytes in the last rows of a full assembly with the GCM tag (see [Format](08_Format.md#data-layout-in-an-assembly)). Archives remain readable with 0.10.1, but files with a block at the end of an assembly fail to restore with a checksum error.

1. check all files with 0.10.1 while the originals still exist: `lxr_restore --verify -x ... -d ... -n ... -i ...` lists every file that fails
2. back up files that failed again into a new database or with a new `-i`; with the same database, deduplication keeps the damaged blocks
