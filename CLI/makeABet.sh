#!/bin/bash
source "$BETTING_GAME/CLI/Utilities/functions.sh"
source "$BETTING_GAME/CLI/Utilities/variables.sh"

NewRandomHash=$BETTING_GAME/Data/NewHash.json
PoolInfo=$BETTING_GAME/Data/PoolInfo.json
BetInfo=$BETTING_GAME/Data/BetInfo_$BetInfoHash.json
Unit=$BETTING_GAME/Data/Unit.json
AMOUNT=2000000
BET_PRICE=100000000
NewJACKPOT=$(expr $JACKPOT + $BET_PRICE)

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
    --tx-in $RandomGenerator_UTXO \
    --tx-in-script-file $RandomGenerator_SCRIPT \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $Unit \
    \
    --tx-in $Betting_UTXO \
    --tx-in-script-file $Betting_SCRIPT \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $Unit \
    \
    --tx-in-collateral $COLLATERAL_UTXO \
    --required-signer-hash $COLLATERAL_PKH \
    \
    --tx-out $RandomGenerator_ADDRESS+$AMOUNT+"1 $RandomNumber_NFT" \
    --tx-out-inline-datum-file $NewRandomHash \
    \
    --tx-out $Betting_ADDRESS+$NewJACKPOT+"1 $Game_NFT" \
    --tx-out-inline-datum-file $PoolInfo \
    \
    --tx-out $Player_ADDRESS+$AMOUNT+"1 $Bet_NFT" \
    --tx-out-inline-datum-file $BetInfo \
    --mint "1 $Bet_NFT" \
    --mint-script-file $PLUTUS_SCRIPTS/BetNFTMintingPolicy.plutus \
    --mint-redeemer-file $BetInfo \
    --change-address $Player_ADDRESS \
    \
    --invalid-before $ValidityStarts \
    --invalid-hereafter $ValidityEnds \
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