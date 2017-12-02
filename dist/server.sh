#!/bin/sh
DIR=$(cd $(dirname $0);pwd)
$DIR/$1 2>> /tmp/server.log
#OCAMLRUNPARAM=b $DIR/server 2>> /tmp/server.log
#node $DIR/../out/src/server.js 2>> /tmp/server.log
