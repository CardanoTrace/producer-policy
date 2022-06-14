#!/bin/bash

cd ../..
echo
echo running tx creation in: $(pwd)
echo
echo "args:" 
echo "[(producerNum) $1]"
echo "[(2 skeyFile) $2]"
echo "[(3 inAddr) $3]"
echo "[(4 outAddr) $4]"
echo "[(5 ppFile) $5]"
echo "[(6) $6]"
echo 

producerNum=$1
if [ "$producerNum" == "" ]; then
echo "producer NFT's number missing"
exit 1
fi

skeyFile=$2
if [ "$skeyFile" == "" ]; then
echo "validator address missing"
exit 1
fi

inAddr=$3
if [ "$inAddr" == "" ]; then
echo "input address missing"
exit 1
fi

outAddr=$4
if [ "$outAddr" == "" ]; then
echo "output address missing"
exit 1
fi

ppFile=$5
if [ "$ppFile" == "" ]; then
echo "protocol paramteres file missing"
exit 1
fi


validatorAddr=$(cardano-cli address build --$testnet --payment-script-file testnet/counter-validator.plutus.json)
if [ "$validatorAddr" == "" ]; then
echo "validator address missing"
exit 1
fi

# only pubKey
# $(cardano-cli address build --payment-verification-key-file $CARDANO/addr/testnet/payment/payment.vkey --$testnet)

# constant, not required for now, TODO add payment to mint
# scOwnerAddress=

# constant
scNFTAssetClass="$(cat testnet/thread-token-policy.hash.txt).4e4654776974746572436f756e7465724f7261636c65"

# constant
valueToMint="1 $(cardano-cli transaction policyid --script-file testnet/producer-policy.plutus.json ).$(echo "Trace identifier #$producerNum" | tr -d '\n' | xxd -ps -c 200)"

echo
echo valueToMint = $valueToMint
echo

utxoCollateral=""
tx_in=""
while read -r utxo; do

    # echo $utxo
    
    in_addr=$(awk '{ print $1 }' <<< "${utxo}" )

    idx=$(awk '{ print $2 }' <<< "${utxo}")

    utxo_balance=$(awk '{ print $3 }' <<< "${utxo}")

    if [ "$utxoCollateral" == "" ]; then
        if [ $(($utxo_balance >= 5000000)) != 0 ]; then
            utxoCollateral="${in_addr}#${idx}"
        else
            tx_in="${tx_in} --tx-in ${in_addr}#${idx}"
        fi
    else
        tx_in="${tx_in} --tx-in ${in_addr}#${idx}"
    fi
done <<< $(cardano-cli query utxo --address $inAddr --$testnet | tail -n +3 | sort --key=3 --numeric-sort) 

scUtxoIn=""
while read -r utxo; do

    scTxInFstAsset=$(awk '{ print $7 }' <<< ${utxo})
    echo scTxInFstAsset: $scTxInFstAsset

    echo scTxInFstAsset == scNFTAssetClass : $(expr "$scTxInFstAsset" == "$scNFTAssetClass" )
    if [ $(expr "$scTxInFstAsset" == "$scNFTAssetClass" ) ]; then
        txRef=$(awk '{ print $1 }' <<< ${utxo})
        txIdx=$(awk '{ print $2 }' <<< ${utxo})
        
        scUtxoIn="$txRef#$txIdx"
        break;
    fi

done <<< $(cardano-cli query utxo --address $validatorAddr --$testnet | tail -n +3 | sort --key=3 --numeric-sort)


if [ "$utxoCollateral" == "" ]; then
echo "!!! unable to find utxoCollateral !!!"
exit 1
fi

if [ "$scUtxoIn" == "" ]; then
echo "!!! unable to find scTxIn !!!"
exit 1
fi

echo smartContract tx input : $scUtxoIn
# echo expected: -------------- 6dd158e9c16cc7295ecc6367cc65e6c819d2c6fe4906c4471a7236cbb23473cc#1


echo
echo outAddr : $outAddr

echo
echo going to run:
echo cardano-cli transaction build 
echo     --$testnet 
echo     --tx-in $utxoCollateral 
echo     ${tx_in} 
echo     --tx-in $scUtxoIn 
echo     --tx-in-script-file "testnet/counter-validator.plutus.json" 
echo     --tx-in-datum-file "testnet/input-data/int$producerNum.json"  
echo     --tx-in-redeemer-file "testnet/input-data/unit.json" 
echo     --tx-in-collateral $utxoCollateral 
echo     
echo     --tx-out "$(cardano-cli address build --$testnet --payment-script-file testnet/counter-validator.plutus.json) + 5000000 lovelace + 1 fcc9ec1a54e173ee9ba7dffd43fc28ddb0a52dcfcf3c718a2e5296c4.4e4654776974746572436f756e7465724f7261636c65" 
echo     --tx-out-datum-embed-file "testnet/input-data/int$(expr $producerNum + 1).json" 
# echo     --tx-out "$scOwnerAddress + 10000000 lovelace" 
echo     --tx-out "$outAddr + 5000000 lovelace + $valueToMint" 
echo     --change-address $inAddr 
echo     
echo     --mint "$valueToMint" 
echo     --mint-script-file "testnet/producer-policy.plutus.json" 
echo     --mint-redeemer-file "testnet/input-data/unit.json" 
echo     --protocol-params-file $ppFile 
echo     --out-file "shell/.tmp/prodId-mint.unsigned.tx"

echo
echo

# !!! IMPORTANT !!!
# include the datum corresponding to the sitting hash
# 
#   --tx-in $scUtxoIn \
#   --tx-in-datum-value "{ \"int\": \"$producerNum\" }" \
#

cardano-cli transaction build \
    --$testnet \
    --tx-in $utxoCollateral \
    ${tx_in} \
    --tx-in $scUtxoIn \
    --tx-in-script-file "testnet/counter-validator.plutus.json" \
    --tx-in-datum-file "testnet/input-data/int$producerNum.json"  \
    --tx-in-redeemer-file "testnet/input-data/unit.json" \
    --tx-in-collateral $utxoCollateral \
    \
    --tx-out "$validatorAddr + 5000000 lovelace + 1 $(cat testnet/thread-token-policy.hash.txt).$(echo "TraceIdsCounterOracle" | tr -d "\n" | xxd -ps -c 200)" \
    --tx-out-datum-embed-file "testnet/input-data/int$(expr $producerNum + 1).json" \
    --tx-out "$outAddr + 5000000 lovelace + $valueToMint" \
    --change-address $inAddr \
    \
    --mint "$valueToMint" \
    --mint-script-file "testnet/producer-policy.plutus.json" \
    --mint-redeemer-file "testnet/input-data/unit.json" \
    --protocol-params-file $ppFile \
    --out-file "shell/.tmp/prodId-mint.unsigned.tx"

echo

# $CARDANO/addr/testnet/payment/payment.skey

echo going to run:
echo cardano-cli transaction sign 
echo     --tx-body-file "shell/.tmp/prodId-mint.unsigned.tx" 
echo     --signing-key-file $skeyFile 
echo     --$testnet 
echo     --out-file "shell/.tmp/prodId-mint.signed.tx"

cardano-cli transaction sign \
    --tx-body-file "shell/.tmp/prodId-mint.unsigned.tx" \
    --signing-key-file $skeyFile \
    --$testnet \
    --out-file "shell/.tmp/prodId-mint.signed.tx"

echo

cardano-cli transaction submit \
    --$testnet \
    --tx-file "shell/.tmp/prodId-mint.signed.tx"
