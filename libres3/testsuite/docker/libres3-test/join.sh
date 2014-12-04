#!/bin/sh
set -e
. /home/build/sxsetup.conf

sed -e "s/SX_NODE_IP.*/SX_NODE_IP=$NODE_1_PORT_443_TCP_ADDR/" /home/build/sxsetup.conf >/home/build/sxsetup.conf.first
libres3_setup --s3-host libres3.skylable.com --s3-port 8443\
    --default-volume-size 10G\
    --default-replica 1\
    --sxsetup-conf /home/build/sxsetup.conf.first\
    --batch

libres3 start
echo "$SX_ADMIN_KEY" >admin.key
sxinit sx://admin@$SX_CLUSTER_NAME --host-list=$NODE_1_PORT_443_TCP_ADDR\
    --batch-mode --auth-file admin.key

SPEC=`sxadm cluster --info sx://admin@$SX_CLUSTER_NAME | grep 'Current' | cut -f2 -d:`
sxadm cluster --mod $SPEC\
    100G/$NODE_2_PORT_443_TCP_ADDR\
    sx://$SX_CLUSTER_NAME

cd /home/build/libres3/libres3/
testsuite/docker/run-test.sh /usr/local/etc/libres3/libres3.sample.s3cfg
