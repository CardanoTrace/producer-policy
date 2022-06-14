#!/bin/bash


../mintProducerId.sh \
    1 \
    testnet/addr/inAddr.skey \
    $(cat ../../testnet/addr/inAddr.addr) \
    $(cat ../../testnet/addr/outAddr.addr) \
    testnet/protocolParams.json
