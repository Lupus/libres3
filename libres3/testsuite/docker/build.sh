#!/bin/sh
set -e
IMG_PREFIX=libres3_test_

(cd ../../../ && git archive --format=tar HEAD) >libres3/libres3.tar

SX_REV=master
SX_URL=http://git.skylable.com/sx
SX_REV=$(git ls-remote $SX_URL $SX_REV | cut -f1)
sed -e "s|@SX_URL@|$SX_URL|g" -e "s|@SX_REV@|$SX_REV|g" sx/Dockerfile.in >sx/Dockerfile

sudo time docker build -t ${IMG_PREFIX}_sx sx
#sudo time docker build -t ${IMG_PREFIX}_libres3 libres3
#sudo time docker build -t ${IMG_PREFIX}_libres3_test libres3-test
