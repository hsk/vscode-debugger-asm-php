#!/bin/sh
DIR=$(cd $(dirname $0);pwd)
$DIR/server.php 2>> /tmp/server.log
#node $DIR/../out/src/server.js 2>> /tmp/server.log
