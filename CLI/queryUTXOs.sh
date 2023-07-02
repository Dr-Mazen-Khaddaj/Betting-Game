#!/bin/bash
source "$BETTING_GAME/CLI/Utilities/variables.sh"

OUT_FILE="$BETTING_GAME/CLI/UTXOs.info"

cardano-cli query utxo \
    --address $RandomGenerator_ADDRESS \
    --testnet-magic 2 \
    --out-file RandomGeneratorSC.info

cardano-cli query utxo \
    --address $Betting_ADDRESS \
    --testnet-magic 2 \
    --out-file BettingSC.info

cardano-cli query utxo \
    --address $Redeeming_ADDRESS \
    --testnet-magic 2 \
    --out-file RedeemingSC.info
# 
echo      "===================================================================================================||"   >   $OUT_FILE
echo "RandomGeneratorSC :"  >>  $OUT_FILE
cat RandomGeneratorSC.info  >>  $OUT_FILE
echo -e "\n===================================================================================================||"   >>  $OUT_FILE
echo "BettingSC : "         >>  $OUT_FILE
cat BettingSC.info          >>  $OUT_FILE
echo -e "\n===================================================================================================||"   >>  $OUT_FILE
echo "RedeemingSC :"        >>  $OUT_FILE
cat RedeemingSC.info        >>  $OUT_FILE
# 
rm RandomGeneratorSC.info
rm BettingSC.info
rm RedeemingSC.info
# -- END OF CODE ----------------------------------------------------------------------------- |