#!/bin/bash

cardano-cli adress build-script \
    --script-file $1 \
    --out-file $2 \
    --testnet-magic 1097977063
    