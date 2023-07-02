#!/bin/bash
source "$BETTING_GAME/CLI/Utilities/functions.sh"
source "$BETTING_GAME/CLI/Utilities/variables.sh"

DATUM=$BETTING_GAME/Data/InitialHash.json
AMOUNT=2000000

echo "============================================================================================================================="

# Cardano-cli commands
cardano-cli transaction build \
    --babbage-era \
    --tx-in $Player_UTXO \
    --tx-out $RandomGenerator_ADDRESS+$AMOUNT+"1 $RandomNumber_NFT" \
    --change-address $Player_ADDRESS \
    --tx-out-inline-datum-file $DATUM \
    --out-file tx.raw \
    --testnet-magic 2
#
cardano-cli transaction sign \
    --tx-file tx.raw \
    --signing-key-file $Player_SKEY \
    --out-file tx.signed \
    --testnet-magic 2
# 
cardano-cli transaction submit \
    --tx-file tx.signed \
    --testnet-magic 2
# 
printTxIdInfo "tx.signed"
# 
rm tx.raw
rm tx.signed
# -- END OF CODE ----------------------------------------------------------------------------- |