#!/bin/sh
set -e
cd /home/build/libres3/libres3
/opt/sx/1/sbin/sxserver restart

/opt/libres3/sbin/libres3_setup --s3-host libres3.skylable.com --s3-port 8443\
    --default-volume-size 10G --default-replica 1 --sxsetup-conf\
    /opt/sx/1/etc/sxserver/sxsetup.conf --batch
/opt/libres3/sbin/libres3 start
$HOME/run-test.sh /opt/libres3/etc/libres3/libres3.sample.s3cfg

/opt/sx/2/sbin/sxserver restart
