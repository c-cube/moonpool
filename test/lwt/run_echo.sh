#!/bin/bash

PORT=1235
./echo_server.exe -p $PORT &

echo "run echo client -p $PORT $@"

export LC_LANG=C
export LC_ALL=C
./echo_client.exe -p $PORT $@ | sort

kill %1
