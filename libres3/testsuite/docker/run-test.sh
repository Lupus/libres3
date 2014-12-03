#!/bin/sh
set -e
S3CMD_BIN=s3cmd
RANDGEN_BIN=randgen

if [ $# -ne 1 ]; then
    echo "Usage: $0 </path/to/libres3.s3cfg>\n" >&2
    exit 1
fi
echo "Running unit tests"
./netTest.native --s3cfg $1 --backtrace -verbose
echo "Running s3cmd tests"

S3CMD="$S3CMD_BIN -c $1"
VOL_PREFIX=vtest.`uuidgen -r`
VOL1=$VOL_PREFIX.v1
VOL2=$VOL_PREFIX.v2
RANDGEN=$RANDGEN_BIN

cleanup() {
    $S3CMD rm -r --force s3://$VOL1 || true
    $S3CMD rm -r --force s3://$VOL2 || true
    $S3CMD rb s3://$VOL1 s3://$VOL2 || true
}

trap cleanup INT TERM EXIT
$S3CMD mb s3://$VOL1
 $S3CMD mb s3://$VOL2
# able to delete and recreate same volume
$S3CMD rb s3://$VOL2
$S3CMD mb s3://$VOL2
# list empty bucket
$S3CMD ls s3://$VOL1
$S3CMD la

upload_download() {
    FILE=test.$1.$2
    $RANDGEN $1 $2 >inputs/$FILE
    $S3CMD put inputs/$FILE $3/
    rm -f $FILE.check
    $S3CMD get $3/$FILE $FILE.check
    cmp $FILE.check inputs/$FILE
}

rm -rf inputs
mkdir inputs
# upload empty file
upload_download 0 0 s3://$VOL2
# small file
upload_download 4096 4096 s3://$VOL2
# delete small file
$S3CMD rm s3://$VOL2/test.4096.4096
# more small files
upload_download 4096 8192 s3://$VOL2
# sync
$S3CMD sync inputs/ s3://$VOL2
$S3CMD sync inputs/ s3://$VOL2
$S3CMD ls s3://$VOL2
rm -rf outputs && mkdir outputs
$S3CMD sync s3://$VOL2 outputs/ --debug
$S3CMD sync s3://$VOL2 outputs/
$S3CMD sync s3://$VOL2 inputs/
diff -ru inputs/ outputs/
# one multipart chunk
MPART_CHUNK=15728640
upload_download $MPART_CHUNK $MPART_CHUNK s3://$VOL1
upload_download $MPART_CHUNK `expr $MPART_CHUNK + $MPART_CHUNK - 1` s3://$VOL1
SX_CHUNK=134217728
upload_download $SX_CHUNK $SX_CHUNK s3://$VOL2
upload_download $SX_CHUNK `expr 3 \* $SX_CHUNK` s3://$VOL2
