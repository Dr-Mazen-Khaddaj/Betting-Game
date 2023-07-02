source "$BETTING_GAME/CLI/Utilities/functions.sh"

# Path Variables
PLUTUS_SCRIPTS=$BETTING_GAME/Scripts
CONTRACT_ADDRESSES=$BETTING_GAME/Addresses/Contract
PAYMENT_ADDRESSES=$BETTING_GAME/Addresses/Payment
COLLATERAL_PATH=$PAYMENT_ADDRESSES/collateral/collateral
Player_PATH=$PAYMENT_ADDRESSES/wallet/wallet

# Collateral
COLLATERAL_UTXO="#"
COLLATERAL_PKH=$(getPublicKeyHash $COLLATERAL_PATH.vkey)
COLLATERAL_SKEY=$COLLATERAL_PATH.skey

# Player
Player_UTXO="#"
Player_ADDRESS=$(cat $Player_PATH.addr)
Player_SKEY=$Player_PATH.skey

# RandomGeneratorSC
RandomGenerator_UTXO="#"
RandomGenerator_ADDRESS=$(cat $CONTRACT_ADDRESSES/RandomGeneratorSC.addr)
RandomGenerator_SCRIPT=$PLUTUS_SCRIPTS/RandomGeneratorSC.plutus

# BettingSC
Betting_UTXO="#"
Betting_ADDRESS=$(cat $CONTRACT_ADDRESSES/BettingSC.addr)
Betting_SCRIPT=$PLUTUS_SCRIPTS/BettingSC.plutus

# RedeemingSC
Redeeming_UTXO1="#"
Redeeming_UTXO2="#"
Redeeming_ADDRESS=$(cat $CONTRACT_ADDRESSES/RedeemingSC.addr)
Redeeming_SCRIPT=$PLUTUS_SCRIPTS/RedeemingSC.plutus

# Current Jackpot @BettingSC
JACKPOT=0

# Bet Info Hash
BetInfoHash=e03bbdccd046b78f8358786aa2495a0662881ec33ab92ae764c133c265260f39

# NFTs
RandomNumber_NFT=27cb0521916d85ad1a304932d8d69a107ca13207569f6a8c0fa4bef1.477265656e2057697a617264
Game_NFT=27cb0521916d85ad1a304932d8d69a107ca13207569f6a8c0fa4bef1.4c55434b5948415348
Bet_NFT=7024cfc792ef99da7cd3b7e772b8d898a689fbb9a00e6fe0e61aee14.$BetInfoHash

# Validity Interval
ValidityStarts=21405385
ValidityEnds=$(expr $ValidityStarts + 300)