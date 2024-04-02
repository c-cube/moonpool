#!/bin/bash

PORT=12345
echo "running hash server on port=$PORT"
./hash_server.exe -p $PORT &
if [ "x$?" != x0 ]; then exit 1 ; fi

sleep 2
echo "run hash client -p $PORT $@"

export LC_LANG=C
export LC_ALL=C
./hash_client.exe -p $PORT $@ | sort

kill %1
