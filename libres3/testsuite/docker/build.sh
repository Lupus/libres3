#!/bin/sh
set -e
IMG_PREFIX=libres3_test_

git -C ../../../ archive --format=tar HEAD >libres3/libres3.tar

sudo time docker build -t ${IMG_PREFIX}_sx sx
sudo time docker build -t ${IMG_PREFIX}_sx_client sx-client
sudo time docker build -t ${IMG_PREFIX}_libres3 libres3
sudo time docker build -t ${IMG_PREFIX}_libres3_test libres3-test
