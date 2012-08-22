#!/bin/bash

# Simple script to run CloudHaskell application on an SMP node

usage() {
  cat - <<EOF
Usage: $0 <nWorkers> <executable> <argument>*
Starts a Cloud Haskell process in <nWorkers> copies on a given machine
<nWorkers> should be at least 2
EOF
  exit 1
}

if [ $# -lt 2 ] || [ $1 -lt 2 ] || [ ! -x $2 ]; then
  usage
fi

# Number of worker processes:
WORKERS=$1
EXE=$2
shift
shift
ARGS=$*
# Executable of CloudHaskell process

# Initialize default config file
cat - >config <<EOF
cfgRole MASTER
cfgHostName localhost
cfgKnownHosts localhost
EOF
#cfgPromiseFlushDelay 0

for i in `seq $WORKERS`; do
  $EXE -cfgRole=WORKER &
done;
sleep 1;
time $EXE $ARGS;
sleep 1;
for i in `seq $WORKERS`; do
  # Try to kill all worker processes
  # [No worry, they tend to die anyway after a while.]
  kill %$i;
done
# Just to be sure... -> hope it doesn't interfere with another process?
#killall $EXE

