#!/bin/sh
#set -e
. $HOME/sxsetup.conf

sed -e "s/SX_NODE_IP.*/SX_NODE_IP=$NODE_1_PORT_443_TCP_ADDR/" $HOME/sxsetup.conf >$HOME/sxsetup.conf.first
libres3_setup --s3-host libres3.skylable.com --s3-http-port 8008 --s3-https-port 8443\
    --default-volume-size 10G\
    --default-replica 1\
    --sxsetup-conf $HOME/sxsetup.conf.first\
    --batch

cd /usr/src/libres3/libres3
libres3_ocsigen --foreground&
echo "$SX_ADMIN_KEY" >admin.key
chmod 0600 admin.key
sxinit sx://admin@$SX_CLUSTER_NAME --host-list=$NODE_1_PORT_443_TCP_ADDR\
    --batch-mode --auth-file admin.key

SPEC=`sxadm cluster --info sx://admin@$SX_CLUSTER_NAME | grep 'Current' | cut -f2 -d:`
sxadm cluster --mod $SPEC\
    100G/$NODE_2_PORT_443_TCP_ADDR\
    sx://$SX_CLUSTER_NAME

eval `opam config env`
#ocaml setup.ml -test
# randgen must not be built with coverage testing enabled
ocamlfind ocamlopt test/randgen.ml -package ocplib-endian,unix -linkpkg -o randgen.native

testsuite/docker/run-test.sh /usr/local/etc/libres3/libres3.sample.s3cfg

kill $!
wait
make cov-report
