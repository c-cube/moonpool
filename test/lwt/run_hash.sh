#!/bin/bash

./hash_server.exe &

echo "run hash client $@"

export LC_LANG=C
export LC_ALL=C
./hash_client.exe $@ | sort

kill %1
