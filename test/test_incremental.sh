#!/bin/bash

# call this script from the root of the project

# this test is backing up a file of around 1 MB size
# then, it incrementally backs up this file again but this time with more data appended at the end
# last, it incrementally backs up the shorter file again

# at every step it restores the file from the archive and verifies its MD5 checksum against the original
# also, we check whether the presence of the encryption keys in the db and the number of blocks


set -E

IRMIN=$(which irmin)

TESTFILE="test1M"
TFILELARGE="test1M.large"
TFILESMALL="test1M.small"

# parameters
   # test101
MYID=7357101
NCHUNKS=16
ELYKSEER_DB=../elykseer.db
ELYKSEER_LXR=../elykseer.chunks
OUTPATH=/tmp

# prepare test files
S="abcdefghjknpqrstuvwxyz0123456789"
SZ=$((1024 * 1024))
CNT=$((SZ / 32))

#set +x
rm -f $TFILESMALL
for (( i=1; i<$CNT; i++ )) do
    echo $S >> $TFILESMALL
done
cp $TFILESMALL $TFILELARGE
for (( i=1; i<1024; i++ )) do
    echo -n $S >> $TFILELARGE
done
#set -x

# filehash of our test file
export FHASH=$(dune exec bin/lxr_filehash.exe -- -f $TESTFILE)

# backup small file first
cp $TFILESMALL $TESTFILE
dune exec bin/lxr_backup.exe --  -v -x $ELYKSEER_LXR -n $NCHUNKS -d $ELYKSEER_DB -j 1 -i $MYID $TESTFILE
MAXBLOCK1=$(irmin get $MYID/relfiles/${FHASH:4:2}/${FHASH} | jq -r '.blocks[] | .blockid' | sort -r | head -1)
echo
AIDs=$(irmin get $MYID/relfiles/${FHASH:4:2}/${FHASH} | jq -r '.blocks[] | .blockaid' | sort | uniq)
for AID in $AIDs; do
   echo -n "checking aid = $AID  "
   if [ $(irmin get $MYID/relkeys/${AID:4:2}/$AID | jq -r '.keys.localid') == $MYID ]
   then echo "√"
   else echo "x"; fi
done
echo
rm -f $OUTPATH/$TESTFILE
dune exec bin/lxr_restore.exe --  -v -x $ELYKSEER_LXR -n $NCHUNKS -d $ELYKSEER_DB -j 1 -i $MYID -o $OUTPATH $TESTFILE
MD5orig=$(md5sum $TFILESMALL | cut -f1 -d ' ')
MD5test=$(md5sum $OUTPATH/$TESTFILE | cut -f1 -d ' ')
[[ $MD5orig == $MD5test ]]

# incrementally backup larger file
cp $TFILELARGE $TESTFILE
dune exec bin/lxr_incremental.exe --  -v -x $ELYKSEER_LXR -n $NCHUNKS -d $ELYKSEER_DB -i $MYID $TESTFILE
MAXBLOCK2=$(irmin get $MYID/relfiles/${FHASH:4:2}/${FHASH} | jq -r '.blocks[] | .blockid' | sort -r | head -1)
[[ $MAXBLOCK2 -gt $MAXBLOCK1 ]]
echo
AIDs=$(irmin get $MYID/relfiles/${FHASH:4:2}/${FHASH} | jq -r '.blocks[] | .blockaid' | sort | uniq)
for AID in $AIDs; do
   echo -n "checking aid = $AID  "
   if [ $(irmin get $MYID/relkeys/${AID:4:2}/$AID | jq -r '.keys.localid') == $MYID ]
   then echo "√"
   else echo "x"; fi
done
echo
rm -f $OUTPATH/$TESTFILE
dune exec bin/lxr_restore.exe --  -v -x $ELYKSEER_LXR -n $NCHUNKS -d $ELYKSEER_DB -j 1 -i $MYID -o $OUTPATH $TESTFILE
MD5orig=$(md5sum $TFILELARGE | cut -f1 -d ' ')
MD5test=$(md5sum $OUTPATH/$TESTFILE | cut -f1 -d ' ')
[[ $MD5orig == $MD5test ]]

# incrementally backup smaller file again
cp $TFILESMALL $TESTFILE
dune exec bin/lxr_incremental.exe --  -v -x $ELYKSEER_LXR -n $NCHUNKS -d $ELYKSEER_DB -i $MYID $TESTFILE
MAXBLOCK3=$(irmin get $MYID/relfiles/${FHASH:4:2}/${FHASH} | jq -r '.blocks[] | .blockid' | sort -r | head -1)
[[ $MAXBLOCK3 -eq $MAXBLOCK1 ]]
echo
AIDs=$(irmin get $MYID/relfiles/${FHASH:4:2}/${FHASH} | jq -r '.blocks[] | .blockaid' | sort | uniq)
for AID in $AIDs; do
   echo -n "checking aid = $AID  "
   if [ $(irmin get $MYID/relkeys/${AID:4:2}/$AID | jq -r '.keys.localid') == $MYID ]
   then echo "√"
   else echo "x"; fi
done
echo
rm -f $OUTPATH/$TESTFILE
dune exec bin/lxr_restore.exe --  -v -x $ELYKSEER_LXR -n $NCHUNKS -d $ELYKSEER_DB -j 1 -i $MYID -o $OUTPATH $TESTFILE
MD5orig=$(md5sum $TFILESMALL | cut -f1 -d ' ')
MD5test=$(md5sum $OUTPATH/$TESTFILE | cut -f1 -d ' ')
[ $MD5orig = $MD5test ]

echo "perfect! all done."
