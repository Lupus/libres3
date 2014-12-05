#!/bin/sh
set -e
IMG_PREFIX=libres3_test_

SX_REV=master
SX_URL=http://git.skylable.com/sx
#SX_REV=$(git ls-remote $SX_URL $SX_REV | cut -f1)
#SX_REV=3f4ed80533187eccdcd0ade8f8b9d2b46b5efb74
SX_REV=e9fa554fdbd512ee5e10e796266a0ecd1e1e3336
SX_REV=1ae5043a4e56aef579ea8af61c3a1b5766bd65b8
SX_REV=2b6a1ed56c3645a605e7205118ad3e77ac13fdd1
sed -e "s|@SX_URL@|$SX_URL|g" -e "s|@SX_REV@|$SX_REV|g" sx/Dockerfile.in >sx/Dockerfile

LIBRES3_REV=${1-origin/master}
LIBRES3_URL=http://git.skylable.com/libres3
LIBRES3_REV=$(git rev-parse --verify "$LIBRES3_REV^{commit}")
sed -e "s|@LIBRES3_URL@|$LIBRES3_URL|g"\
    -e "s|@LIBRES3_REV@|$LIBRES3_REV|g"\
    libres3/Dockerfile.in >libres3/Dockerfile

sudo time docker build -t ${IMG_PREFIX}_sx sx
sudo time docker build -t ${IMG_PREFIX}_sx_client sx-client
sudo time docker build -t ${IMG_PREFIX}_libres3 libres3
sudo time docker build -t ${IMG_PREFIX}_libres3_test libres3-test
