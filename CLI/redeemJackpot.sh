#!/bin/bash
source "$BETTING_GAME/CLI/Utilities/functions.sh"
source "$BETTING_GAME/CLI/Utilities/variables.sh"

BetInfo=$BETTING_GAME/Data/BetInfo_$BetInfoHash.json
AMOUNT=2000000

echo "============================================================================================================================="

# Protocol Parameters
cardano-cli query protocol-parameters \
    --out-file protocol.params \
    --testnet-magic 2

# Cardano-cli commands
cardano-cli transaction build \
    --babbage-era \
    --tx-in $Player_UTXO \
    \
    --tx-in $Redeeming_UTXO1 \
    --tx-in-script-file $Redeeming_SCRIPT \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $BetInfo \
    --tx-in $Redeeming_UTXO2 \
    --tx-in-script-file $Redeeming_SCRIPT \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $BetInfo \
    \
    --tx-in-collateral $COLLATERAL_UTXO \
    --required-signer-hash $COLLATERAL_PKH \
    \
    --tx-out $Player_ADDRESS+$AMOUNT+"1 $Bet_NFT" \
    --change-address $Player_ADDRESS \
    \
    --protocol-params-file protocol.params \
    --out-file tx.raw \
    --testnet-magic 2
#
cardano-cli transaction sign \
    --tx-file tx.raw \
    --signing-key-file $Player_SKEY \
    --signing-key-file $COLLATERAL_SKEY \
    --out-file tx.signed \
    --testnet-magic 2
# 
cardano-cli transaction submit \
    --tx-file tx.signed \
    --testnet-magic 2
# 
printTxIdInfo "tx.signed"
# 
rm protocol.params
rm tx.raw
rm tx.signed
# -- END OF CODE ----------------------------------------------------------------------------- |