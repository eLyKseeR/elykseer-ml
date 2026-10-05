# Examples

A walkthrough of all tools. The `sh` blocks are executed in this order by `test/test_examples.sh` (in CI: IT workflow), so they must keep working. They assume the `lxr_*` tools, `irmin` and `jq` are on the `PATH` (as in the Docker image), and work in the current directory.

setup
=====

```sh
MYID=test1
ELYKSEER_db=${PWD}/elykseer.db
ELYKSEER_chunks=${PWD}/elykseer.chunks
mkdir -p ${ELYKSEER_chunks}
```

The database is created by the first backup. To inspect it with the _irmin_ command line tool, configure it in `irmin.yml`:

```sh
cat << EOF > irmin.yml
root: ${ELYKSEER_db}
store: git
contents: json-value
EOF
```


lxr_backup
==========

create some test data:

```sh
dd if=/dev/urandom of=test1M bs=1M count=1 2>/dev/null
dd if=/dev/urandom of=test4M bs=1M count=4 2>/dev/null
dd if=/dev/urandom of=test8M bs=1M count=8 2>/dev/null
md5sum test1M test4M test8M > md5sums
```

back up the files, then look at the meta data of one file:

```sh
lxr_backup -v -x ${ELYKSEER_chunks} -d ${ELYKSEER_db} -n 16 -i $MYID test1M test4M test8M
FHASH=$(lxr_filehash -f test1M -i ${MYID} | cut -d ' ' -f 2)

irmin list ${MYID}/relfiles/${FHASH:4:2}

irmin get ${MYID}/relfiles/${FHASH:4:2}/${FHASH} | jq -r '
  .blocks[] | [.blockaid,.blockapos,.filepos,.blocksize] | @csv' | awk "{print \"${FHASH},\"\$0}" > relfiles.csv
```


lxr_restore
===========

```sh
mkdir -p restored
lxr_restore -v -x ${ELYKSEER_chunks} -d ${ELYKSEER_db} -n 16 -o restored -i $MYID test1M test8M test4M
```

test the restored files:

```sh
(cd restored && md5sum -c ../md5sums)
```

verify all files of the identifier without keeping them (restores into a temporary directory and compares the checksums):

```sh
lxr_restore --verify -x ${ELYKSEER_chunks} -d ${ELYKSEER_db} -n 16 -i $MYID
```


lxr_relfiles and lxr_relkeys
============================

the blocks of the files (CSV), and the keys of their assemblies:

```sh
lxr_relfiles -d ${ELYKSEER_db} -i $MYID test8M test4M test1M
lxr_relkeys -d ${ELYKSEER_db} -i $MYID test8M test4M test1M
```

the assembly id of the first block of a file:

```sh
AID=$(lxr_relfiles -d ${ELYKSEER_db} -i $MYID test1M | sed -n 2p | cut -d, -f6 | tr -d '"')
echo $AID
```


lxr_chunks
==========

the paths of the chunks of an assembly:

```sh
lxr_chunks -a $AID -x ${ELYKSEER_chunks} -n 16 -i $MYID
```


lxr_distribute
==============

A sink configuration with an S3/MinIO and a filesystem sink, `sinks.json`:
```json
{
  "version": "1.0.0",
  "sinks": [
    {
        "type": "S3",
        "name": "s3_minio",
        "description": "minio storage cluster",
        "credentials": {
            "access-key-env": "MINIO_ACCESS_KEY",
            "secret-key-env": "MINIO_SECRET_KEY"
        },
        "access": {
            "bucket": "lxr",
            "prefix": "lxr",
            "host": "minio.example.com",
            "port": "9000",
            "protocol": "https"
        }
    },
    {
        "type": "FS",
        "name": "fs_copy",
        "description": "filesystem copy",
        "credentials": {
            "user": "*",
            "group": "root",
            "permissions": "640"
        },
        "access": {
            "basepath": "/data/secure_stick"
        }
    }
  ]
}
```

Credentials:
- S3 requests are signed with AWS SigV4 (by curl); this works with AWS S3 and MinIO.
- instead of inline secrets (`"access-key"`, `"secret-key"`), `"access-key-env"` and `"secret-key-env"` name environment variables that hold the secrets; these take precedence.
- a configuration file with inline secrets must not be readable by group or others: `chmod 600 sinks.json`
- only `https` is accepted as protocol, `http` only for `localhost`/loopback.
- `--insecure` disables the last two checks.

An invalid sink in the configuration (unknown type, missing field, rejected protocol) makes `lxr_distribute` exit with code 2; a chunk that could not be copied makes it exit with code 1.

Here, with two filesystem sinks: copy half of the chunks of the assembly to each:

```sh
mkdir -p sink1 sink2
cat << EOF > sinks-fs.json
{ "sinks": [
  { "type": "FS", "name": "sink1", "credentials": { "user": "*" }, "access": { "basepath": "${PWD}/sink1" } },
  { "type": "FS", "name": "sink2", "credentials": { "user": "*" }, "access": { "basepath": "${PWD}/sink2" } } ] }
EOF

lxr_distribute -v -d PUT -n 16 -x ${ELYKSEER_chunks} -i $MYID -a $AID -c sinks-fs.json 8 8
```

and get them back into an empty chunk directory, then restore from there:

```sh
lxr_distribute -d GET -n 16 -x ${PWD}/chunks.copy -i $MYID -a $AID -c sinks-fs.json 8 8
mkdir -p restored.copy
lxr_restore -x ${PWD}/chunks.copy -d ${ELYKSEER_db} -n 16 -o restored.copy -i $MYID test1M
cmp test1M restored.copy/test1M
```
