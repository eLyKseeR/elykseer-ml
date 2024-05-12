irmin setup
===========

```sh
eval $(opam env)

mkdir -vp ${HOME}/elykseer.chunks
mkdir -vp ${HOME}/elykseer.db

cat << EOF > irmin.yml
root: ${HOME}/elykseer.db
store: git
contents: json-value
EOF
```

`irmin init`


lxr_backup
==========

```sh
MYID=test1

dd if=/dev/random of=test1M bs=1M count=1
dd if=/dev/random of=test4M bs=1M count=4
dd if=/dev/random of=test8M bs=1M count=8
md5sum test[148]M > md5sums
```

```sh
dune exec lxr_backup -- -v -x ${HOME}/elykseer.chunks -d ${HOME}/elykseer.db -n 16 -i $MYID test1M test4M test8M
FHASH=$(dune exec lxr_filehash -- -f test1M -i ${MYID} | cut -d ' ' -f 2)

irmin list ${MYID}/relfiles/${FHASH:4:2}

irmin get ${MYID}/relfiles/${FHASH:4:2}/${FHASH} | jq -r '
  .blocks[] | [.blockaid,.blockapos,.filepos,.blocksize] | @csv' | awk "{print \"${FHASH},\"\$0}" >> relfiles.csv
```


lxr_restore
===========

```sh
rm -fv /tmp/test[148]M
dune exec lxr_restore -- -v -d ${HOME}/elykseer.db -x ${HOME}/elykseer.chunks -n 16 -o /tmp/ -i $MYID test1M test8M test4M
```

test the restored files
```sh
cd /tmp/
md5sum -c ${HOME}/md5sums
```


lxr_chunks
==========

dune exec bin/lxr_chunks.exe -- -a 0274252e09e6ff36a712d7e693824486bf408fd681935725b5fcdf8c8d51a49f -x lxr -n 16 -i $MYID


lxr_relkeys
===========

dune exec bin/lxr_relkeys.exe -- -v -i $MYID -d ../elykseer.db test8M test4M testCHAR test1M

