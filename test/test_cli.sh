#!/usr/bin/env bash
# Command line tests of the lxr_* tools: exit codes on success and on
# failure, and that a failed restore leaves no partial file behind.
# Run by `dune test` (see test/dune), or directly:
#   bash test/test_cli.sh _build/default/bin

set -u

BIN="$(cd "${1:-_build/default/bin}" && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "${WORK}"' EXIT
cd "${WORK}"

MYID=clitest
NFAIL=0

# expect <exit code> <description> <command...>
expect() {
  local want=$1 desc=$2; shift 2
  "$@" > out.log 2>&1
  local got=$?
  if [ "$got" -eq "$want" ]; then
    echo "  ok    ${desc}"
  else
    echo "  FAIL  ${desc}: exit code ${got}, expected ${want}"
    sed 's/^/        | /' out.log
    NFAIL=$((NFAIL + 1))
  fi
}

check() {
  local desc=$1; shift
  if "$@"; then echo "  ok    ${desc}"; else echo "  FAIL  ${desc}"; NFAIL=$((NFAIL + 1)); fi
}

backup()  { "${BIN}/lxr_backup.exe" -x chunks -d db -n 16 -i "${MYID}" "$@"; }
restore() { "${BIN}/lxr_restore.exe" -x chunks -d db -i "${MYID}" "$@"; }
aid_of()  { "${BIN}/lxr_relfiles.exe" -d db -i "${MYID}" "$1" | sed -n 2p | cut -d, -f6 | tr -d '"'; }
chunks_of() { find chunks -name '*.lxr' -newer "$1" | sort; }

head -c 300000 /dev/urandom > a.bin
head -c 70000 /dev/urandom > b.bin

echo "lxr_backup"
expect 2 "missing -x/-d is a usage error" "${BIN}/lxr_backup.exe" a.bin
expect 2 "-n below 16 is a usage error" "${BIN}/lxr_backup.exe" -x chunks -d db -n 8 -i "${MYID}" a.bin
expect 2 "-n above 256 is a usage error" "${BIN}/lxr_backup.exe" -x chunks -d db -n 300 -i "${MYID}" a.bin
expect 0 "backup of two files" backup a.bin b.bin
expect 1 "backup of a missing file fails" backup missing.bin
touch stamp
head -c 50000 /dev/urandom > c.bin
expect 1 "backup with one missing file fails" backup c.bin missing.bin
mkdir -p o0 && expect 0 "the readable file was still backed up" restore -o o0 c.bin

echo "lxr_restore"
expect 2 "missing -x/-d is a usage error" "${BIN}/lxr_restore.exe" -o o0 a.bin
mkdir -p o1
expect 0 "restore of two files" restore -o o1 a.bin b.bin
check "restored files are identical" cmp -s a.bin o1/a.bin
check "restored files are identical" cmp -s b.bin o1/b.bin
mkdir -p o2
expect 1 "restore with a different identifier finds no meta data" \
  "${BIN}/lxr_restore.exe" -x chunks -d db -i other -o o2 a.bin
expect 1 "restore of an existing file fails" restore -o o1 a.bin
expect 2 "restore from a missing database" "${BIN}/lxr_restore.exe" -x chunks -d nodb -i "${MYID}" -o o2 a.bin
check "no database is created" test ! -e nodb

# c.bin lives alone in the last assembly (written after "stamp")
CHUNKS_C=$(chunks_of stamp)
FIRST_C=$(echo "${CHUNKS_C}" | head -n1)
cp "${FIRST_C}" saved.lxr
printf '\x55' | dd of="${FIRST_C}" bs=1 seek=1000 conv=notrunc 2>/dev/null
mkdir -p o3
expect 1 "restore from a corrupted chunk fails" restore -o o3 c.bin
expect 1 "verify detects the corrupted chunk" restore --verify c.bin
check "no partial file after corrupted chunk" test ! -e o3/c.bin
rm -f "${FIRST_C}"
expect 1 "restore with a missing chunk fails" restore -o o3 c.bin
check "no partial file after missing chunk" test ! -e o3/c.bin
cp saved.lxr "${FIRST_C}"
expect 0 "restore after repairing the chunk" restore -o o3 c.bin
check "repaired restore is identical" cmp -s c.bin o3/c.bin
expect 0 "verify all files" restore --verify
grep -q "verified 3 of 3 files" <("${BIN}/lxr_restore.exe" -v --verify -x chunks -d db -i "${MYID}") ; check "verify covers all 3 files" test $? -eq 0

echo "meta data errors"
if [ "$(id -u)" -ne 0 ]; then
  cp -R db db.ro && chmod -R a-w db.ro
  head -c 1000 /dev/urandom > d.bin
  expect 1 "backup into a read-only database fails" "${BIN}/lxr_backup.exe" -x chunks -d db.ro -n 16 -i "${MYID}" d.bin
  chmod -R u+w db.ro
else
  echo "  skip  read-only database (running as root)"
fi

echo "incremental backup"
S="abcdefghjknpqrstuvwxyz0123456789"
for i in $(seq 1 2000); do echo "$S"; done > inc.small
cp inc.small inc.large
for i in $(seq 1 4000); do printf '%s' "$S"; done >> inc.large
nblocks() { "${BIN}/lxr_relfiles.exe" -d db -i "${MYID}" inc.bin | tail -n +2 | wc -l | tr -d ' '; }
inc_roundtrip() {
  local desc=$1 orig=$2
  rm -rf oi && mkdir oi
  expect 0 "${desc}: restore" restore -o oi inc.bin
  check "${desc}: restored file is identical" cmp -s "${orig}" oi/inc.bin
}
cp inc.small inc.bin
expect 0 "backup small file" backup inc.bin
N1=$(nblocks)
inc_roundtrip "small" inc.small
cp inc.large inc.bin
expect 0 "backup grown file" backup inc.bin
N2=$(nblocks)
check "grown file has more blocks (${N1} -> ${N2})" test "${N2}" -gt "${N1}"
inc_roundtrip "grown" inc.large
NCHUNKS_BEFORE=$(find chunks -name '*.lxr' | wc -l)
expect 0 "backup unchanged file" backup inc.bin
check "unchanged file writes no new chunks" test "$(find chunks -name '*.lxr' | wc -l)" -eq "${NCHUNKS_BEFORE}"
cp inc.small inc.bin
expect 0 "backup shrunk file" backup inc.bin
check "shrunk file has the original number of blocks" test "$(nblocks)" -eq "${N1}"
inc_roundtrip "shrunk" inc.small

echo "lxr_distribute"
AID=$(aid_of c.bin)
mkdir -p sink1 sink2
cat > sinks.json <<EOF
{ "sinks": [
  { "type": "FS", "name": "s1", "credentials": { "user": "*" }, "access": { "basepath": "${WORK}/sink1" } },
  { "type": "FS", "name": "s2", "credentials": { "user": "*" }, "access": { "basepath": "${WORK}/sink2" } } ] }
EOF
distribute() { "${BIN}/lxr_distribute.exe" -a "${AID}" -n 16 -i "${MYID}" "$@"; }
expect 2 "missing -a/-x/-c is a usage error" "${BIN}/lxr_distribute.exe" 8 8
expect 0 "PUT to two filesystem sinks" distribute -d PUT -x chunks -c sinks.json 8 8
check "all 16 chunks distributed" test "$(find sink1 sink2 -name '*.lxr' | wc -l | tr -d ' ')" -eq 16
expect 0 "PUT again skips existing chunks" distribute -d PUT -x chunks -c sinks.json 8 8
expect 0 "GET into an empty chunk directory" distribute -d GET -x chunks2 -c sinks.json 8 8
same_chunks() {
  local n=0 f
  for f in $(cd chunks2 && find . -name '*.lxr'); do
    cmp -s "chunks/$f" "chunks2/$f" || return 1
    n=$((n + 1))
  done
  [ "$n" -eq 16 ]
}
check "16 chunks fetched, identical to the originals" same_chunks
expect 2 "weights that miss chunks are a usage error" distribute -d PUT -x chunks -c sinks.json 4 8
expect 2 "fewer weights than sinks are a usage error" distribute -d PUT -x chunks -c sinks.json 16
expect 2 "more weights than sinks are a usage error" distribute -d PUT -x chunks -c sinks.json 8 8 8
expect 0 "weights above nchunks keep copies on several sinks" distribute -d PUT -x chunks -c sinks.json 16 16
rm -rf sink2
expect 1 "PUT to a missing sink directory fails" distribute -d PUT -x chunks -c sinks.json 8 8
echo '{ "sinks": [ { "type": "FTP", "name": "x" } ] }' > bad.json
expect 2 "invalid sink configuration" distribute -d PUT -x chunks -c bad.json 16

echo "audit regressions"
# chunks that cannot all be written must fail the backup (half of the chunk subdirectories are read-only)
if [ "$(id -u)" -ne 0 ]; then
  mkdir -p chunks.part
  for i in $(seq 0 255); do x=$(printf %02x "$i"); mkdir "chunks.part/$x"; [ "$i" -lt 128 ] && chmod 555 "chunks.part/$x"; done
  head -c 5000 /dev/urandom > p.bin
  expect 1 "backup with unwritable chunks fails" "${BIN}/lxr_backup.exe" -x chunks.part -d db.part -n 16 -i "${MYID}" p.bin
  chmod -R u+w chunks.part
else
  echo "  skip  unwritable chunks (running as root)"
fi

# the chunk count is taken from the key, so -n may change between backups and on restore
head -c 300000 /dev/urandom > mix.bin
expect 0 "backup with -n 16" "${BIN}/lxr_backup.exe" -x chunks.mix -d db.mix -n 16 -i "${MYID}" mix.bin
head -c 300000 /dev/urandom >> mix.bin
expect 0 "backup grown file with -n 32" "${BIN}/lxr_backup.exe" -x chunks.mix -d db.mix -n 32 -i "${MYID}" mix.bin
mkdir -p omix
expect 0 "restore of mixed -n assemblies without -n" "${BIN}/lxr_restore.exe" -x chunks.mix -d db.mix -i "${MYID}" -o omix mix.bin
check "mixed -n restore is identical" cmp -s mix.bin omix/mix.bin
grep -q "obsolete" <("${BIN}/lxr_restore.exe" --verify -x chunks.mix -d db.mix -n 32 -i "${MYID}" mix.bin 2>&1); check "restore warns that -n is obsolete" test $? -eq 0
expect 0 "restore still accepts the obsolete -n" "${BIN}/lxr_restore.exe" --verify -x chunks.mix -d db.mix -n 32 -i "${MYID}" mix.bin

# a missing key fails only the files that need it
if command -v git > /dev/null; then
  head -c 1000 /dev/urandom > k1.bin
  head -c 1000 /dev/urandom > k2.bin
  "${BIN}/lxr_backup.exe" -x chunks.key -d db.key -n 16 -i "${MYID}" k1.bin > /dev/null 2>&1
  "${BIN}/lxr_backup.exe" -x chunks.key -d db.key -n 16 -i "${MYID}" k2.bin > /dev/null 2>&1
  KAID=$("${BIN}/lxr_relfiles.exe" -d db.key -i "${MYID}" k1.bin | sed -n 2p | cut -d, -f6 | tr -d '"')
  # CI has no git identity configured
  (export GIT_INDEX_FILE="${WORK}/key.idx" GIT_AUTHOR_NAME=test GIT_AUTHOR_EMAIL=test@localhost \
          GIT_COMMITTER_NAME=test GIT_COMMITTER_EMAIL=test@localhost; G="git --git-dir=db.key/.git"
   set -e; $G read-tree main; $G rm -q --cached -r "${MYID}/relkeys/${KAID:4:2}/${KAID}"
   $G update-ref refs/heads/main "$($G commit-tree "$($G write-tree)" -p main -m "drop key")") > /dev/null 2>&1
  check "key removed from the database" test $? -eq 0
  mkdir -p okey
  expect 1 "restore with a missing key fails without crashing" "${BIN}/lxr_restore.exe" -x chunks.key -d db.key -i "${MYID}" -o okey k1.bin k2.bin
  check "file with a known key is still restored" cmp -s k2.bin okey/k2.bin
  check "file with the missing key is not restored" test ! -e okey/k1.bin
  expect 0 "lxr_relkeys with a missing key does not crash" "${BIN}/lxr_relkeys.exe" -d db.key -i "${MYID}" k1.bin
else
  echo "  skip  missing key (no git)"
fi

# a meta data entry that points to the wrong place in the assembly is detected
if command -v git > /dev/null; then
  head -c 1000 /dev/urandom > w1.bin
  head -c 1000 /dev/urandom > w2.bin
  "${BIN}/lxr_backup.exe" -x chunks.wrong -d db.wrong -n 16 -i "${MYID}" w1.bin w2.bin > /dev/null 2>&1
  W1APOS=$("${BIN}/lxr_relfiles.exe" -d db.wrong -i "${MYID}" w1.bin | sed -n 2p | cut -d, -f7)
  W2HASH=$("${BIN}/lxr_relfiles.exe" -d db.wrong -i "${MYID}" w2.bin | sed -n 2p | cut -d, -f2 | tr -d '"')
  # w2's block now points to w1's data
  (export GIT_INDEX_FILE="${WORK}/wrong.idx" GIT_AUTHOR_NAME=test GIT_AUTHOR_EMAIL=test@localhost \
          GIT_COMMITTER_NAME=test GIT_COMMITTER_EMAIL=test@localhost; G="git --git-dir=db.wrong/.git"
   set -e; P="${MYID}/relfiles/${W2HASH:4:2}/${W2HASH}"; $G read-tree main
   BLOB=$($G cat-file -p "main:${P}" | sed -E "s/\"blockapos\":\"[0-9]+\"/\"blockapos\":\"${W1APOS}\"/" | $G hash-object -w --stdin)
   $G update-index --cacheinfo "100644,${BLOB},${P}"
   $G update-ref refs/heads/main "$($G commit-tree "$($G write-tree)" -p main -m "wrong apos")") > /dev/null 2>&1
  check "meta data points to the wrong block" test $? -eq 0
  mkdir -p owrong
  expect 1 "restore with a wrong block position fails" "${BIN}/lxr_restore.exe" -x chunks.wrong -d db.wrong -i "${MYID}" -o owrong w2.bin
  check "no file restored from the wrong block" test ! -e owrong/w2.bin
else
  echo "  skip  wrong block position (no git)"
fi

# the meta data of a backup is written in one commit for the keys and one for the files
if command -v git > /dev/null; then
  head -c 1000 /dev/urandom > c1.bin; head -c 1000 /dev/urandom > c2.bin; head -c 1000 /dev/urandom > c3.bin
  expect 0 "backup of three files" "${BIN}/lxr_backup.exe" -x chunks.commits -d db.commits -n 16 -i "${MYID}" c1.bin c2.bin c3.bin
  check "meta data written in 2 commits" test "$(git --git-dir=db.commits/.git rev-list --count main)" -eq 2
  mkdir -p ocommits
  expect 0 "restore of the three files" "${BIN}/lxr_restore.exe" -x chunks.commits -d db.commits -i "${MYID}" -o ocommits c1.bin c2.bin c3.bin
  check "three files restored identical" cmp -s c3.bin ocommits/c3.bin
else
  echo "  skip  meta data commits (no git)"
fi

# symlinks are not followed in a recursive backup
mkdir -p loop.src && head -c 1000 /dev/urandom > loop.src/f && ln -s . loop.src/loop
expect 0 "recursive backup with a symlink loop" "${BIN}/lxr_backup.exe" -x chunks.loop -d db.loop -n 16 -i "${MYID}" -D loop.src -R
check "symlink loop is not entered" test "$(grep -c 'recurse into' out.log)" -eq 1

if [ "${NFAIL}" -gt 0 ]; then
  echo "${NFAIL} command line test(s) failed"
  exit 1
fi
echo "all command line tests passed"
