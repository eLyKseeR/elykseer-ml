#!/usr/bin/env bash

set -e

eval $(opam env)

export PATH=${HOME}/.local/bin:$PATH

MYID=test1

dd if=/dev/random of=test1M bs=1M count=1
dd if=/dev/random of=test4M bs=1M count=4
dd if=/dev/random of=test8M bs=1M count=8
md5sum test[148]M > md5sums
md5sum -c md5sums

lxr_backup -v -x ${HOME}/elykseer.chunks -d ${HOME}/elykseer.db -n 16 -i $MYID test1M test4M test8M
FHASH=$(lxr_filehash -f test1M -i ${MYID} | cut -d ' ' -f 2)

irmin list ${MYID}/relfiles/${FHASH:4:2}

rm -fv /tmp/test[148]M
lxr_restore -v -d ${HOME}/elykseer.db -x ${HOME}/elykseer.chunks -n 16 -o /tmp/ -i $MYID test1M test8M test4M

cd /tmp/
md5sum -c ${HOME}/md5sums
cd ${HOME}

echo
echo "test succeeded."
