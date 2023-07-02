#!/bin/bash
source "$BETTING_GAME/CLI/Utilities/functions.sh"
source "$BETTING_GAME/CLI/Utilities/variables.sh"

# RandomGeneratorSC
cardano-cli address build \
    --payment-script-file $PLUTUS_SCRIPTS/RandomGeneratorSC.plutus \
    --out-file $CONTRACT_ADDRESSES/RandomGeneratorSC.addr \
    --testnet-magic 2

# BettingSC
cardano-cli address build \
    --payment-script-file $PLUTUS_SCRIPTS/BettingSC.plutus \
    --out-file $CONTRACT_ADDRESSES/BettingSC.addr \
    --testnet-magic 2

# RedeemingSC
cardano-cli address build \
    --payment-script-file $PLUTUS_SCRIPTS/RedeemingSC.plutus \
    --out-file $CONTRACT_ADDRESSES/RedeemingSC.addr \
    --testnet-magic 2
