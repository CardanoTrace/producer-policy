#!/bin/bash


address=$1
skeyFile=$2
vkeyFile=$3
ppFile=$4

echo address: $address
if [ "$address" == "" ]; then
echo "address missing"
exit 1
fi

echo skeyFile: $skeyFile
if [ "$skeyFile" == "" ]; then
echo "skey file path missing"
exit 1
fi

echo vkeyFile: $vkeyFile
if [ "$vkeyFile" == "" ]; then
echo "skey file path missing"
exit 1
fi


echo ppFile: $ppFile
if [ "$ppFile" == "" ]; then
echo "protocol-parameter file path missing"
exit 1
fi

echo

addr_publicKeyHash=$(cardano-cli address key-hash --payment-verification-key-file ../../$vkeyFile ) # 82341640c593d46b6df46f0e59f2cc2827d04c536d525dca926d561a for $CARDANO/addr/testnet/payment/payment.vkey
tokenName="TraceIdsCounterOracle"

# cardano-cli query utxo --address $address --$testnet | tail -n +3 | sort --key=3 --numeric-sort # --reverse # for descending

utxoToSpend=""
tx_in=""
while read -r utxo; do

    # echo $utxo
    
    in_addr=$(awk '{ print $1 }' <<< "${utxo}" )

    idx=$(awk '{ print $2 }' <<< "${utxo}")

    utxo_balance=$(awk '{ print $3 }' <<< "${utxo}")

    if [ "$utxoToSpend" == "" ]; then
        if [ $(($utxo_balance >= 5000000)) != 0 ]; then
            utxoToSpend="${in_addr}#${idx}"
        else
            tx_in="${tx_in} --tx-in ${in_addr}#${idx}"
        fi
    else
        tx_in="${tx_in} --tx-in ${in_addr}#${idx}"
    fi
done <<< $(cardano-cli query utxo --address $address --$testnet | tail -n +3 | sort --key=3 --numeric-sort) 


echo TxOut: $utxoToSpend
echo lovelaces: ${utxo_balance} "( $((${utxo_balance}/1000000)).$((${utxo_balance}%1000000)) ADA )"

echo

cd ../..
echo running in: $(pwd)
echo

echo running: cabal run -- make-all $addr_publicKeyHash $utxoToSpend $tokenName
cabal run -- make-all $addr_publicKeyHash $utxoToSpend $tokenName

echo

tokenNameHex=$(echo ${tokenName} | tr -d "\n" | xxd -ps -c 200)

valueToMint="1 $(cat testnet/thread-token-policy.hash.txt).$tokenNameHex"

echo "$(cardano-cli address build --$testnet --payment-script-file testnet/counter-validator.plutus.json) + $valueToMint"
#save address
cardano-cli address build --$testnet --payment-script-file testnet/counter-validator.plutus.json > testnet/$(cat testnet/thread-token-policy.hash.txt)-validatorCounter.addr 
echo

cardano-cli transaction build \
    --$testnet \
    --tx-in $utxoToSpend \
    ${tx_in} \
    --tx-in-collateral $utxoToSpend \
    \
    --tx-out "$(cardano-cli address build --$testnet --payment-script-file testnet/counter-validator.plutus.json) + 5000000 lovelace + $valueToMint" \
    --tx-out-datum-embed-file "testnet/input-data/int1.json" \
    --change-address $address \
    \
    --mint "$valueToMint" \
    --mint-script-file "testnet/thread-token-policy.plutus.json" \
    --mint-redeemer-file "testnet/input-data/unit.json" \
    --protocol-params-file $ppFile \
    --out-file "$(pwd)/shell/.tmp/mint.unsigned.tx" \

echo

# $CARDANO/addr/testnet/payment/payment.skey

cardano-cli transaction sign \
    --tx-body-file "$(pwd)/shell/.tmp/mint.unsigned.tx" \
    --signing-key-file $skeyFile \
    --$testnet \
    --out-file "$(pwd)/shell/.tmp/mint.signed.tx"

echo

cardano-cli transaction submit \
    --$testnet \
    --tx-file "$(pwd)/shell/.tmp/mint.signed.tx"
