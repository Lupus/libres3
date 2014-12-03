#!/bin/sh
set -e
if [ $# -ne 1 ]; then
    echo "Usage: $0 <git revision>\n"
    exit 1
fi
cd $HOME/libres3/libres3
git remote update
git checkout $1
eval `opam config env`
./configure --disable-docs --enable-tests --prefix=/opt/libres3 --override ocamlbuildflags '-j 0'
make test install
