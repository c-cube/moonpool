#!/bin/bash

PORT_FILE=$(mktemp)
rm -f "$PORT_FILE"
echo "running hash server"
./hash_server.exe -p 0 -port-file "$PORT_FILE" &
if [ "x$?" != x0 ]; then exit 1 ; fi

for _ in $(seq 100); do [ -s "$PORT_FILE" ] && break; sleep 0.1; done
PORT=$(cat "$PORT_FILE")
rm -f "$PORT_FILE"
echo "run hash client $@"

export LC_LANG=C
export LC_ALL=C
./hash_client.exe -p $PORT $@ | sort

kill %1
