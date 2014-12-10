#!/bin/sh
set -e
SX_CLUSTER_NAME=localhost
IMG_PREFIX=libres3_test_

FIRST_NODE=${IMG_PREFIX}_sx_1
sudo docker rm --force $FIRST_NODE || true

PREFIX=/usr
cat >sxsetup.conf <<EOF
SX_CLUSTER_NAME="$SX_CLUSTER_NAME"
SX_DATA_DIR="/var/lib/sxserver/storage"
SX_RUN_DIR="/var/run/sxserver"
SX_LIB_DIR="/var/lib/sxserver"
SX_LOG_FILE="/var/log/sxserver/sxfcgi.log"
SX_NODE_SIZE="100G"
SX_NODE_INTERNAL_IP=""
SX_SERVER_USER="nobody"
SX_SERVER_GROUP="nogroup"
SX_CHILDREN_NUM="32"
SX_NODE_IP=\`hostname -I | cut -f1 -d\ \`
SX_PORT="443"
SX_USE_SSL="yes"
SX_SSL_KEY="`cat cluster1.key`"
SX_SSL_KEY_FILE=/etc/ssl/private/sxkey.pem
SX_SSL_CERT="`cat cluster1.pem`"
SX_SSL_CERT_FILE=/etc/ssl/certs/sxcert.pem
SX_CFG_VERSION="2"
SX_CLUSTER_UUID=39de275d-79c0-460c-8af0-4197902e3b33
SX_ADMIN_KEY=0DPiKuNIrrVmD8IUCuw1hQxNqZfdYEcoLRkvmAwVYZH2KdCayJdv7QAA
SX_CLUSTER_KEY=CLUSTER/ALLNODE/ROOT/USERwDEr6NGSlYDjE9JDYHmEm06E7h3UQAA
EOF

sudo docker run -d -t -i\
    --name $FIRST_NODE\
    -v `pwd`/sxsetup.conf:/root/sxsetup.conf:ro\
    ${IMG_PREFIX}_sx /bin/sh

sudo docker exec $FIRST_NODE\
    $PREFIX/sbin/sxsetup --config-file /root/sxsetup.conf --wait&

N=4
for i in `seq 2 $N`; do
    sudo docker rm --force ${IMG_PREFIX}_sx_$i || true
    sudo docker run -d -t -i\
        --name ${IMG_PREFIX}_sx_$i\
        -v `pwd`/sxsetup.conf:/root/sxsetup.conf:ro\
        -v `pwd`/admin.key:/root/admin.key:ro\
        --link $FIRST_NODE:first\
        ${IMG_PREFIX}_sx /bin/sh
    sudo docker exec ${IMG_PREFIX}_sx_$i\
        $PREFIX/sbin/sxsetup --config-file /root/sxsetup.conf --bare&
done

sudo docker rm --force ${IMG_PREFIX}_sx_client || true
sudo docker run -d -t -i\
        --name ${IMG_PREFIX}_sx_client\
        -v `pwd`/sxsetup.conf:/root/sxsetup.conf:ro\
        -v `pwd`/admin.key:/root/admin.key:ro\
        --link $FIRST_NODE:first\
        --link ${IMG_PREFIX}_sx_2:node_2\
        --link ${IMG_PREFIX}_sx_3:node_3\
        --link ${IMG_PREFIX}_sx_4:node_4\
        ${IMG_PREFIX}_sx_client /bin/sh

sudo docker rm --force ${IMG_PREFIX}_libres3 || true
sudo docker run -d -t -i\
    --name ${IMG_PREFIX}_libres3\
    -v `pwd`/sxsetup.conf:/home/build/sxsetup.conf:ro\
    --link $FIRST_NODE:first\
    --link ${IMG_PREFIX}_sx_2:node_2\
    --link ${IMG_PREFIX}_sx_3:node_3\
    --link ${IMG_PREFIX}_sx_4:node_4\
    ${IMG_PREFIX}_libres3 /bin/sh

sudo docker exec ${IMG_PREFIX}_libres3\
    sh -x -c '\
        sed -e "s/SX_NODE_IP.*/SX_NODE_IP=$FIRST_PORT_443_TCP_ADDR/"\
            /home/build/sxsetup.conf >/home/build/sxsetup.conf.first &&\
        libres3_setup --s3-host libres3.skylable.com --s3-port 8443\
            --default-volume-size 10G\
            --default-replica 1\
            --sxsetup-conf /home/build/sxsetup.conf.first\
            --batch'&

wait
# sudo docker exec ${IMG_PREFIX}_sx_client /home/build/sx-node-ops.sh

#sudo docker exec ${IMG_PREFIX}_libres3\
#    cat /usr/local/etc/libres3/libres3.sample.s3cfg >libres3.sample.s3cfg

sudo docker rm --force ${IMG_PREFIX}_libres3_test || true
sudo docker run -t -i\
    --name ${IMG_PREFIX}_libres3_test\
    -v `pwd`/sxsetup.conf:/home/build/sxsetup.conf:ro\
    --link $FIRST_NODE:node_1\
    --link ${IMG_PREFIX}_sx_2:node_2\
    --link ${IMG_PREFIX}_sx_3:node_3\
    --link ${IMG_PREFIX}_sx_4:node_4\
    ${IMG_PREFIX}_libres3_test
