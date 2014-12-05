#!/bin/sh
set -e
IMG_PREFIX=libres3_test_

SX_REV=master
SX_URL=http://git.skylable.com/sx
SX_REV=$(git ls-remote $SX_URL $SX_REV | cut -f1)
sed -e "s|@SX_URL@|$SX_URL|g" -e "s|@SX_REV@|$SX_REV|g" sx/Dockerfile.in >sx/Dockerfile

cp sx-client/Dockerfile.in sx-client/Dockerfile

sudo time docker build -t ${IMG_PREFIX}_sx sx
sudo time docker build -t ${IMG_PREFIX}_sx_client sx-client
