#!/bin/sh
set -e
# configure 4 nodes and setup 1
ADMIN_KEY=0DPiKuNIrrVmD8IUCuw1hQxNqZfiOHEKH5jcMy5Q9/zqDujiINPF/QAA
for i in `seq 1 4`; do
    PREFIX=/opt/sx/$i
    mkdir -p $PREFIX/etc/ssl/private $PREFIX/etc/ssl/certs
    cp /home/build/sx/server/test/keys/cluster1.key $PREFIX/etc/ssl/private/sxkey.pem
    cp /home/build/sx/server/test/keys/cluster1.pem  $PREFIX/etc/ssl/certs/sxcert.pem
    CONF=$PREFIX/etc/sxserver/sxsetup.conf
    cat >$CONF <<EOF
SX_CLUSTER_NAME="localhost"
SX_DATA_DIR="$PREFIX/var/lib/sxserver/storage"
SX_RUN_DIR="$PREFIX/var/run/sxserver"
SX_LIB_DIR="$PREFIX/var/lib/sxserver"
SX_LOG_FILE="$PREFIX/var/log/sxserver/sxfcgi.log"
SX_NODE_SIZE="1T"
SX_NODE_IP="127.0.1.$i"
SX_NODE_INTERNAL_IP=""
SX_SERVER_USER="build"
SX_SERVER_GROUP="build"
SX_CHILDREN_NUM="32"
SX_PORT="9443"
SX_USE_SSL="yes"
SX_SSL_KEY_FILE="$PREFIX/etc/ssl/private/sxkey.pem"
SX_SSL_CERT_FILE="$PREFIX/etc/ssl/certs/sxcert.pem"
SX_CFG_VERSION="2"
SX_CLUSTER_UUID=ae11f6ce-1788-4f47-8ab7-17310cebe274
SX_ADMIN_KEY=$ADMIN_KEY
EOF
    if [ $i -gt 1 ]; then
	echo "SX_EXISTING_NODE_IP=\"127.0.1.1\"" >> $CONF
        /opt/sx/$i/sbin/sxsetup --config-file $CONF --advanced --bare --wait
    else
        /opt/sx/$i/sbin/sxsetup --config-file $CONF --advanced --wait
    fi
done
