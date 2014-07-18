#!/bin/sh
set -e
SXDIR=../../sx
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
echo
echo "Installing ocsigen"
make reinstall
$sbindir/libres3_ocsigen --version
echo "Configuring ocsigen"
mkdir -p $localstatedir/run

conf=$SXDIR/server/test-sx/1/etc/sxserver/sxsetup.conf </dev/null
echo "LIBRES3_PORT=8443" >>$conf
$sbindir/libres3_setup --s3-host libres3.skylable.com --default-replica 1 --sxsetup-conf $conf </dev/null
$sbindir/libres3_ocsigen --stop
echo "list_cache_expires=0." >>$sysconfdir/libres3/libres3.conf
$sbindir/libres3_ocsigen
echo "Running tests"
./netTest.native --s3cfg $sysconfdir/libres3/libres3.sample.s3cfg --backtrace 2>&1 | tee sx.log
echo "OK"
