#!/bin/bash

PORT=1235
./echo_server.exe -p $PORT &

echo "run echo client -p $PORT $@"

./echo_client.exe -p $PORT $@

kill %1
