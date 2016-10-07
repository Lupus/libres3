#!/bin/bash
set -e
git ls-files -- src/{auth,http,io,live}/*.ml{,i}  | xargs headache -h _header.small
git ls-files -- src/sx/*.ml{,i} | xargs headache -h _header.client
git ls-files -- src/{s3,cli,server}/*.ml{,i} | xargs headache -h _header.server
git ls-files -- test/*.ml{,i} | xargs headache -h _header.small
