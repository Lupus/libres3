#!/bin/bash
OBJECT_PATH="$1"
ETCDIR=`opam config var etc`
EXPIRES=$(date +%s --date "now 5 minutes")
EXPIRES=2399971783
AWS_ACCESS_KEY_ID=admin
STRINGTOSIGN=$(printf "GET\n\n\n$EXPIRES\n/$OBJECT_PATH")
SIGNATURE=$(s3cmd -c $ETCDIR/libres3/libres3.sample.s3cfg sign "$STRINGTOSIGN")
SIGNATURE=${SIGNATURE#Signature: }
SIGNATURE="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$SIGNATURE")"
echo "$STRINGTOSIGN"
echo "curl -v -k -O 'https://libres3.skylable.com:8443/$OBJECT_PATH?AWSAccessKeyId=$AWS_ACCESS_KEY_ID&Expires=$EXPIRES&Signature=$SIGNATURE'"
STRINGTOSIGN=$(printf "PUT\n\n\n$EXPIRES\n/$OBJECT_PATH")
SIGNATURE=$(s3cmd -c $ETCDIR/libres3/libres3.sample.s3cfg sign "$STRINGTOSIGN")
SIGNATURE=${SIGNATURE#Signature: }
SIGNATURE="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$SIGNATURE")"
echo "$STRINGTOSIGN"
echo "curl -v -k -T /path/to/source -O 'https://libres3.skylable.com:8443/$OBJECT_PATH?AWSAccessKeyId=$AWS_ACCESS_KEY_ID&Expires=$EXPIRES&Signature=$SIGNATURE'"
