#!/bin/sh
set -e
cd ..
SXDIR=../../sx
if true; then
make distclean
PREFIX=`pwd`/test-libres3
$PREFIX/sbin/libres3_ocsigen --stop || true
rm -rf "$PREFIX"
mkdir "$PREFIX"
./configure --prefix="$PREFIX" --enable-tests --disable-docs
make

. ./setup.data
cleanup() {
    echo "Killing libres3_ocsigen"
    $sbindir/libres3_ocsigen --stop
}
trap cleanup INT TERM EXIT
echo "Starting SX"
(cd $SXDIR/server && test/start-nginx-ssl.sh)
else
. ./setup.data
fi
echo
echo "Installing ocsigen"
make reinstall
$sbindir/libres3_ocsigen --version
echo "Configuring ocsigen"

conf=$SXDIR/server/test-sx/1/etc/sxserver/sxsetup.conf
$sbindir/libres3_setup --s3-host libres3.skylable.com --s3-http-port 8008 --s3-https-port 8443 --default-volume-size 100G --default-replica 1 --sxsetup-conf $conf --batch
echo "list_cache_expires=0." >>$sysconfdir/libres3/libres3.conf
$sbindir/libres3_ocsigen
echo "Running tests"
testsuite/docker/run-test.sh $sysconfdir/libres3/libres3.sample.s3cfg 2>&1 | tee sx.log
echo "OK"
