#!/bin/bash

PORT=1234
./hash_server.exe -p $PORT &

echo "run hash client -p $PORT $@"

export LC_LANG=C
export LC_ALL=C
./hash_client.exe -p $PORT $@ | sort

kill %1
