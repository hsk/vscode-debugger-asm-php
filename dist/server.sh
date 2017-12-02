#!/bin/sh
DIR=$(cd $(dirname $0);pwd)
$1 $DIR/$2 2>> /tmp/server.log
#OCAMLRUNPARAM=b $DIR/server 2>> /tmp/server.log
#node $DIR/../out/src/server.js 2>> /tmp/server.log
