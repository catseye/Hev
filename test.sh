#!/bin/sh

./build.sh || exit 1

FILES="tests/Hev.markdown"
if [ x$USE_HUGS = x ]; then
    FILES="$FILES tests/Internals.markdown"
fi

falderal $FILES
