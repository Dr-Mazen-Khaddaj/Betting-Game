#!/bin/bash
source "$BETTING_GAME/CLI/Utilities/variables.sh"

function QUERY_UTXOs ()                 { cardano-cli query utxo --address $1 --testnet-magic 2;}
function PRINT_Address_UTXOs ()         {
                                            echo -e "${BIWhite}Address:${NC} $1\n"
                                            QUERY_UTXOs $2
                                            echo -e $NEW_LINE
                                        }
function PRINT_Script_UTXOs ()          {
                                            echo -e "${BIYellow}Script:${NC} $1\n"
                                            QUERY_UTXOs $2
                                            echo -e $NEW_LINE
                                        }
function PRINT_SmartContract_UTXOs ()   {
                                            echo -e "${BIBlue}Smart Contract:${NC} $1\n"
                                            QUERY_UTXOs $2
                                            echo -e $NEW_LINE
                                        }

LINE="\n============================================================================================================================================================================="
NEW_LINE="\n\n=============================================================================================================================================================================\n"

BIWhite='\033[1;97m'
BIYellow='\033[1;93m'
BIBlue='\033[1;94m'
NC='\033[0m' # No Color

clear
echo -e $LINE

###############################################################################################

# Wallet
PRINT_Address_UTXOs         "Player's Wallet"   $Player_ADDRESS

# RandomGeneratorSC
PRINT_SmartContract_UTXOs   RandomGeneratorSC   $RandomGenerator_ADDRESS

# BettingSC
PRINT_SmartContract_UTXOs   BettingSC           $Betting_ADDRESS

# RedeemingSC
PRINT_SmartContract_UTXOs   RedeemingSC         $Redeeming_ADDRESS

###############################################################################################