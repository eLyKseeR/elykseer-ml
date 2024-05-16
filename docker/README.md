
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


### restore

restore a single file from chunks:
```sh
lxr_restore.exe -v -x ${HOME}/elykseer.chunks -d ${HOME}/elykseer.db -o /restore/ -i $MYID /data/that_one_important.file
```

see the file:
```sh
ls -lh /restore/data/that_one_important.file

sha
```