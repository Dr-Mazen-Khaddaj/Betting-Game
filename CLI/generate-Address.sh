#!/bin/bash
DIR=$1$2
NAME="$DIR"/"$2"
mkdir $DIR
# Cardano Cli commands ----------------------------------- |
cardano-cli address key-gen \
    --verification-key-file $NAME.vkey \
    --signing-key-file $NAME.skey
# --
 cardano-cli stake-address key-gen \
    --verification-key-file "$NAME"_stake.vkey \
    --signing-key-file "$NAME"_stake.skey
# --
cardano-cli address build \
    --payment-verification-key-file $NAME.vkey \
    --stake-verification-key-file "$NAME"_stake.vkey \
    --out-file $NAME.addr \
    --testnet-magic 2
# -------------------------------------------------------- |
echo "All files were successfully created in the following directory:"
realpath $DIR