#!/bin/sh
set -e
/usr/local/sbin/libres3_setup --s3-host libres3.skylable.com --s3-port 8443\
    --default-volume-size 10G --default-replica 1 --sxsetup-conf\
    /opt/sx/1/etc/sxserver/sxsetup.conf --batch
