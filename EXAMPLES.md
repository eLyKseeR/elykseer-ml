
lxr_backup
==========

MYID=424242

dune exec lxr_backup -- -v -x ../elykseer.chunks -d ../elykseer.db -n 16 -i $MYID test1M test4M test8M
dune exec bin/lxr_filehash.exe -- -f test1M

irmin list ${MYID}/relfiles/${FHASH:4:2}

irmin get ${MYID}/relfiles/${FHASH:4:2}/${FHASH} | jq -r '
  .blocks[] | [.blockaid,.blockapos,.filepos,.blocksize] | @csv' | awk "{print \"${FHASH},\"\$0}" >> relfiles.csv


lxr_processor
=============

dune exec bin/lxr_processor.exe -- -v -x ../elykseer.chunks -d ../elykseer.db -i ${MYID} -n 17 test4M test1M test8M


lxr_restore
===========

dune exec bin/lxr_restore.exe -- -v -d ../elykseer.db -x ../elykseer.chunks -n 16 -o /tmp/ -i $MYID test1M test4M


lxr_incremental
===============

dune exec bin/lxr_incremental.exe -- -v -y -d ../elykseer.db -x ../elykseer.chunks -n 16 -i $MYID test1M


lxr_chunks
==========

dune exec bin/lxr_chunks.exe -- -a 0274252e09e6ff36a712d7e693824486bf408fd681935725b5fcdf8c8d51a49f -x lxr -n 16 -i $MYID


lxr_relkeys
===========

dune exec bin/lxr_relkeys.exe -- -v -i $MYID -d ../elykseer.db test8M test4M testCHAR test1M

