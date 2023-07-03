# Lottery Game on the Blockchain

This project implements a lottery game on the blockchain using Cardano and Plutus. The game utilizes smart contracts, NFTs, and a random number generation mechanism to ensure fairness and transparency. Traditional lotteries often suffer from concerns related to transparency, trust, and fairness. With this project, we address these challenges by harnessing the immutability and decentralized nature of the blockchain. Participants can place bets, compete for the jackpot, and redeem their winnings securely.

## Table of Contents

- [Lottery Game on the Blockchain](#lottery-game-on-the-blockchain)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Getting Started](#getting-started)
  - [Folder Structure](#folder-structure)
  - [Smart Contracts](#smart-contracts)
  - [NFTs](#nfts)

## Overview

This lottery game project aims to provide a decentralized and secure platform for participants to engage in a fair and transparent lottery experience. It utilizes smart contracts, NFTs, and a random number generation mechanism to ensure the integrity of the game.

## Getting Started

To get started with the lottery game project, please follow the steps outlined below:

1. Clone the repository to your local machine.
2. Set the folder Path as an environment variable "BETTING_GAME"
3. Restart the terminal or run "source ~/.bashrc" (or .zshrc)
4. Start Nix.
5. Navigate to repository folder: cd $BETTING_GAME
6. Navigate to Code/App then run cabal repl.
7. Open a second terminal.
8. Navigate to repository folder: cd $BETTING_GAME
9. Navigate to CLI where you can run bash scripts to interact with the game.
10. Open the current directory using an editor: code .
11. Run Cardano Node (preview)
12. Check Test_Example/Walkthrough.md for more details.

## Folder Structure

The project repository has the following folder structure:

- **/Code**: Contains the Plutus code.
  - **/App**: Contains the Smart Contracts and related functionalities.
    - **/src**:
        - `RandomGeneratorSC`: Holds the random number as a hash and shuffles it securely.
        - `BettingSC`: Secures the jackpot and manages the betting process.
        - `RedeemingSC`: Allows winners to redeem their winnings securely.
        - `BetNFTMintingPolicy`: Validates the integrity of the minted `Bet_NFT`.
        - `DataTypes`: Defines the custom Data types and instances used.
        - `Variables`: Defines some Variables to be set before generating Plutus scripts.
        - `HelperFunctions`: Contains common Inlinable functions.
        - `Main`: Contains the Off-Chain part: generating Plutus scripts, and Off-chain helper functions to write Data that will be used during transaction construction..
    - `Main.info`: is generated when running main, it contains Plutus Scripts' Info.
  - **/Utilities**: Contains utility functions used in the project. Copied from the Plutus Pioneer Program.
- **/CLI**: Contains the bash scripts for interacting with the smart contracts and participating in the game.
  - `buildSCAddresses.sh`: Builds the addresses for the smart contracts.
  - `initializeBettingGame.sh`: Initializes the first Betting UTXOs at the BettingSC with the specified PoolInfo (Sessions 0 and 1).
  - `makeABet.sh`: Allows a participant to place a bet in the current live session.
  - `queryAll.sh`: Queries all UTXOs (Player + SC) at the terminal.
  - `queryUTXOs.sh`: Queries SC UTXOs to an output file "UTXO.info".
  - `remove-Address.sh`: Removes an address.
  - `generate-Address.sh`: Generates a new address.
  - `initializeRandomGenerator.sh`: Initializes the RandomGenerator smart contract UTXO with an initial hash.
  - `makeFirstBet.sh`: Allows a participant to create and place a bet at a new live session (N + 2), consuming the old UTXO of session N.
  - `queryTip.sh`: Queries the current tip of the blockchain.
  - `redeemJackpot.sh`: Allows the winner to redeem the jackpot.
  - `shuffle.sh`: Shuffles the random number in the RandomGenerator smart contract.

  - **/Utilities**: Contains utility scripts and files used by the CLI scripts.
  - `functions.sh`: Contains common functions used by the CLI scripts.
  - `variables.sh`: Contains common variables used by the CLI scripts.


- **/Data**: Contains JSON files used for Datum and Redeemer in the transaction construction.
- **/Scripts**: Contains Plutus scripts used in the project.
- **/Test_Example**: Contains a testing example of the app.

## Smart Contracts

The lottery game project consists of three smart contracts:

1. **RandomGeneratorSC**: Holds the random number as a hash at the Datum and shuffles it securely. This contract ensures that the random number cannot be manipulated by participants, maintaining the fairness of the game.

2. **BettingSC**: Secures the jackpot and manages the betting process. It verifies shuffling the random number at the `RandomGeneratorSC` and ensures the correct amount is deposited into the jackpot. It also locks the UTXO after the deadline and verifies the transfer of the jackpot to the `RedeemingSC` after the Locking period ends.

3. **RedeemingSC**: Allows winners to redeem their winnings securely. It verifies the bet information provided by the winner using the token name of the corresponding `Bet_NFT`, ensuring the validity of the bet.

## NFTs

This project utilizes three different NFTs, each serving a specific purpose in the lottery game. The NFTs are as follows:

1. **Random_NFT**: Held by the `RandomGeneratorSC` UTXO for reference purposes.
   - The `Random_NFT` represents a pointer to the Random Generator UTXO.
   - It is held by the `RandomGeneratorSC`'s UTXO.
   - Used by the `BettingSC` smart contract to identify the UTXO that holds the Random Number that is being used by the game.

2. **Game_NFT**: Held by a pair of consecutive game sessions at the BettingSC.
   - The `Game_NFT` is a two of a kind NFT that represents a pair of consecutive game sessions.
   - It is held by two consecutive UTXOs at the `BettingSC` that belong to the same game but have session numbers `x` and `x+1`.
   - When the first UTXO is locked, with session number `x`, the second UTXO is live, representing the game session with session number `x+1`.
   - Once the second UTXO becomes locked, indicating the end of the game session with session number `x+1`, the first UTXO is matured.
   - Subsequently, the first UTXO, with session number `x` is consumed, to generate a new live UTXO with session number `x+2`.
   - This pattern ensures a continuous and sequential flow of game sessions.

3. **Bet_NFT**: Held by the player. Represents a lottery ticket or bet made by a participant.
   - The `Bet_NFT` represents a lottery ticket or a bet made by a participant in the lottery game.
   - It holds the player's bet, the game they are betting on (represented by the currency symbol and token name), and the session number.
   - The information is held as a hash in the token name of the `Bet_NFT`.
   - If the player is a winner, they need to provide the bet information as a redeemer data for the `RedeemingSC` to use.
   - The validity of the information is checked using the token name of the `Bet_NFT`, ensuring the authenticity and integrity of the provided bet information.
