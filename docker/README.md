
## example run with Docker


### create Docker volumes

list the volumes:
```sh
docker volume list
```

if not yet available, create these disk volumes:
```sh
docker volume create elykseer_db
docker volume create elykseer_chunks
```


### run the container

`$ARCH` indicates the platform (arm64 | amd64)
`$SRCDIR` points to a directory that will be mounted readonly into the container at /data; this will be the data to backup
`$TGTDIR` points to a directory which will contain the restored files

```sh
ARCH=$(uname -m)
SRCDIR=/Users/alex/Documents
TGTDIR=/tmp/test_restore

docker run -it --rm \
  -v elykseer_db:/home/coq/elykseer.db \
  -v elykseer_chunks:/home/coq/elykseer.chunks \
  --mount type=bind,source="${SRCDIR}",target=/data,readonly \
  --mount type=bind,source="${TGTDIR}",target=/restore \
  codieplusplus/elykseer-ml-binaries:${ARCH}

in the container:

check the permissions on these directories
```sh
ls -ld elykseer.chunks elykseer.db
```

if necessary, then change with:
```sh
sudo chgrp coq elykseer.chunks elykseer.db
sudo chmod g+ws elykseer.chunks elykseer.db
```


### backup

set an identifier for the backup:
```sh
MYID=test42
```

run a shallow backup on the directory and its content:
```sh
lxr_backup -v -x ${HOME}/elykseer.chunks -d ${HOME}/elykseer.db -n 16 -i $MYID -D /data/
```

check that the files are there:
```sh
irmin list ${MYID}/relfiles
```

### check meta data on a file

```sh
FNAME=/data/that_one_important.file
FHASH=$(dune exec lxr_filehash -- -f ${FNAME} -i ${MYID} | cut -d ' ' -f 2)
irmin list ${MYID}/relfiles/${FHASH:4:2}
```

this should list the file's meta data in irmin.

pretty-print the meta data:

```sh
irmin get ${MYID}/relfiles/${FHASH:4:2}/${FHASH} | jq
```

this show the JSON nice printed:
```json
{
  "version": {
    "major": "0",
    "minor": "9",
    "build": "12"
  },
  "fileinformation": {
    "fname": "/data/that_one_important.file",
    "fhash": "d3524477..7b6e9cc07",
    "fsize": "64798
...
```

### restore

restore a single file from chunks:
```sh
FNAME=/data/that_one_important.file

lxr_restore -v -x ${HOME}/elykseer.chunks -d ${HOME}/elykseer.db -n 16 -o /restore/ -i $MYID ${FNAME}
```

see the file:
```sh
ls -lh /restore/data/that_one_important.file

sha
```