#!/bin/bash

PORT=1235
./echo_server.exe -p $PORT &
if [ "x$?" != x0 ]; then exit 1 ; fi


sleep 0.9
echo "run echo client -p $PORT $@"

export LC_LANG=C
export LC_ALL=C
./echo_client.exe -p $PORT $@ | sort

kill %1
