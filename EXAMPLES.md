
setup
=====

```sh
MYID=test1
ELYKSEER_db=${HOME}/elykseer.db
ELYKSEER_chunks=${HOME}/elykseer.chunks
mkdir -vp ${HOME}/elykseer.chunks
mkdir -vp ${HOME}/elykseer.db
```

irmin setup
===========

```sh
eval $(opam env)

cat << EOF > irmin.yml
root: ${ELYKSEER_db}
store: git
contents: json-value
EOF
```

`irmin init`


lxr_backup
==========

```sh

dd if=/dev/random of=test1M bs=1M count=1
dd if=/dev/random of=test4M bs=1M count=4
dd if=/dev/random of=test8M bs=1M count=8
md5sum test[148]M > md5sums
```

```sh
dune exec lxr_backup -- -v -x ${ELYKSEER_chunks} -d ${ELYKSEER_db} -n 16 -i $MYID test1M test4M test8M
FHASH=$(dune exec lxr_filehash -- -f test1M -i ${MYID} | cut -d ' ' -f 2)

irmin list ${MYID}/relfiles/${FHASH:4:2}

irmin get ${MYID}/relfiles/${FHASH:4:2}/${FHASH} | jq -r '
  .blocks[] | [.blockaid,.blockapos,.filepos,.blocksize] | @csv' | awk "{print \"${FHASH},\"\$0}" >> relfiles.csv
```


lxr_restore
===========

```sh
rm -fv /tmp/test[148]M
dune exec lxr_restore -- -v -x ${ELYKSEER_chunks} -d ${ELYKSEER_db} -n 16 -o /tmp/ -i $MYID test1M test8M test4M
```

test the restored files
```sh
cd /tmp/
md5sum -c ${HOME}/md5sums
```


lxr_chunks
==========

```sh
AID=2dc83988899ffefbacac9b7d1f29cf530940053fc16376b101e3390b1b135079
dune exec bin/lxr_chunks.exe -- -a $AID -x ${ELYKSEER_chunks} -n 16 -i $MYID
```

lxr_relkeys
===========

```sh
dune exec bin/lxr_relkeys.exe -- -v -i $MYID -d ${ELYKSEER_db} test8M test4M test1M
```

lxr_distribute
==============

file `sinks.json`:
```json
{
  "version": "1.0.0",
  "sinks": [
    {
        "type": "MINIO",
        "name": "s3_minio",
        "description": "minio storage cluster",
        "credentials": {
            "access-key": "minioadmin",
            "secret-key": "s3cr3t"
        },
        "access": {
            "bucket": "lxr",
            "host": "localhost",
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

copy half of the chunks of the assembly to each storage locations:
```sh
AID=<something>

dune exec bin/lxr_distribute.exe -- -v -d PUT -n 16 -x ${ELYKSEER_chunks} -i $MYID -a $AID -c sinks.json 8 8
```
