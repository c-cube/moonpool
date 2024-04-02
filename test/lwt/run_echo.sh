#!/bin/bash

PORT=12346
echo "run echo server on port=$PORT"
./echo_server.exe -p $PORT &
if [ "x$?" != x0 ]; then exit 1 ; fi


sleep 2
echo "run echo client -p $PORT $*"

export LC_LANG=C
export LC_ALL=C
./echo_client.exe -p $PORT "$@" | sort

kill %1
