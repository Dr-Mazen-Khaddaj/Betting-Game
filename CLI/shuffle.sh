#!/bin/bash
source "$BETTING_GAME/CLI/Utilities/functions.sh"
source "$BETTING_GAME/CLI/Utilities/variables.sh"

NEWDATUM=$BETTING_GAME/Data/NewHash.json
REDEEMER=$BETTING_GAME/Data/Unit.json
AMOUNT=2000000

# Executing

echo "============================================================================================================================="

# Protocol Parameters
cardano-cli query protocol-parameters \
    --out-file protocol.params \
    --testnet-magic 2

# Cardano-cli commands
cardano-cli transaction build \
    --babbage-era \
    --tx-in $Player_UTXO \
    --tx-in $RandomGenerator_UTXO \
    --tx-in-script-file $RandomGenerator_SCRIPT \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $REDEEMER \
    --tx-in-collateral $COLLATERAL_UTXO \
    --required-signer-hash $COLLATERAL_PKH \
    --tx-out $RandomGenerator_ADDRESS+$AMOUNT+"1 $RandomNumber_NFT" \
    --tx-out-inline-datum-file $NEWDATUM \
    --change-address $Player_ADDRESS \
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