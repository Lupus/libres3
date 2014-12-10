#!/bin/sh
set -e
IMG_PREFIX=libres3_test_

cp sx/Dockerfile.in sx/Dockerfile
cp sx-client/Dockerfile.in sx-client/Dockerfile

LIBRES3_REV=${1-origin/master}
LIBRES3_URL=http://git.skylable.com/libres3
LIBRES3_REV=$(git rev-parse --verify "$LIBRES3_REV^{commit}")
sed -e "s|@LIBRES3_URL@|$LIBRES3_URL|g"\
    -e "s|@LIBRES3_REV@|$LIBRES3_REV|g"\
    libres3/Dockerfile.in >libres3/Dockerfile
cp libres3-test/Dockerfile.in libres3-test/Dockerfile

sudo time docker build -t ${IMG_PREFIX}_sx sx
sudo time docker build -t ${IMG_PREFIX}_sx_client sx-client
sudo time docker build -t ${IMG_PREFIX}_libres3 libres3
sudo time docker build -t ${IMG_PREFIX}_libres3_test libres3-test
