#!/bin/sh

cd $HOME
. $HOME/sxsetup.conf
get_dist() {
    DIST=`sxadm cluster --info sx://admin@$SX_CLUSTER_NAME|grep Current|cut -f2 -d:`
}

wait_sx () {
    while true; do
        sxadm cluster --info sx://admin@$SX_CLUSTER_NAME >tmp ||\
            { cat tmp; sleep 1; continue; }
        if grep 'State of op' tmp >/dev/null; then
            echo
            echo "Waiting for $1"
            cat tmp
            sleep 1
            continue
        else
            break
        fi
    done
    echo "Finished $1"
}

get_dist

DIST=`echo "$DIST" |cut -f3 -d\ |
    sed -e "s/$NODE_2_PORT_443_TCP_ADDR/$NODE_3_PORT_443_TCP_ADDR/"`

echo
echo "STEP: --replace-faulty node3 replaces faulty node2, this succeeds"
echo

sxadm cluster --replace-faulty $DIST sx://admin@$SX_CLUSTER_NAME
wait_sx

echo
echo "STEP: check with --info that there are no operations in progress"
echo

sxadm cluster --info sx://admin@$SX_CLUSTER_NAME

echo
echo "STEP: try the previous --replace-faulty command again"
echo "STEP: expected to fail with 'This node is active' but it doesn't"
echo "STEP: instead it fails with connection error"
echo

sxadm cluster --replace-faulty $DIST sx://admin@$SX_CLUSTER_NAME
wait_sx

echo
echo "STEP: try it again, now it fails with node is locked error"
echo
sxadm cluster --replace-faulty $DIST sx://admin@$SX_CLUSTER_NAME
wait_sx

echo
echo "STEP: check with --info that there are no operations in progress"
echo

sxadm cluster --info sx://admin@$SX_CLUSTER_NAME
echo
echo "STEP: try it again, now it fails with node is locked error"
echo
sxadm cluster --replace-faulty $DIST sx://admin@$SX_CLUSTER_NAME
wait_sx
