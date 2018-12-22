#!/bin/sh
DIR=$(cd $(dirname $0);pwd)
echo $1 $DIR/$2 >> /tmp/server.log
OCAMLRUNPARAM=b $1 $DIR/$2 2>> /tmp/server.log
# $DIR/server 2>> /tmp/server.log
#node $DIR/../out/src/server.js 2>> /tmp/server.log
