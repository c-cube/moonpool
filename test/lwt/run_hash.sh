#!/bin/bash

./hash_server.exe &

echo "run hash client $@"
./hash_client.exe $@ | sort

kill %1
