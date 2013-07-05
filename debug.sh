#!/usr/bin/env bash

MYDIR=$(dirname "$0")
MYDIR=$(cd "$MYDIR" && pwd -L)

ERL=erl
which werl >/dev/null 2>&1 && ERL=werl

if ! [ -f "$MYDIR/app.config" ]; then
    cp -p "$MYDIR/rel/files/app.config" "$MYDIR"
    echo "A template app.config has been created.  Edit to taste and re-run" >&2
    echo "$0 when finished." >&2
    exit 1
fi

mkdir -p "$MYDIR/log/sasl"
$ERL -pa "$MYDIR/ebin" "$MYDIR"/deps/*/ebin \
    -config app \
    -name tcstream \
    -run tcstream debug \
    -- enable_demo
