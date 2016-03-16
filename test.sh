#!/bin/sh

./build.sh || exit 1
bin/hev test || exit 1
FILES="tests/Hev.markdown"
falderal $FILES
