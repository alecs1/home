#!/bin/bash
date
echo "Starting calls"
./sync-tcp-dt-client.exe 10.58.10.224 2>&1 &
./sync-tcp-dt-client.exe 10.58.10.224 2>&1 &
./sync-tcp-dt-client.exe 10.58.10.224 2>&1 &
./sync-tcp-dt-client.exe 10.58.10.224 2>&1 &
./sync-tcp-dt-client.exe 10.58.10.224 2>&1 &
wait

echo "Done"
date
