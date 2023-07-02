{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns    #-}

module RedeemingSC where

import      PlutusTx                    ( compile )
import      PlutusTx.Prelude            ( Bool (False) , Maybe (Just, Nothing) , Eq ((==)) , Ord ((>))
                                        , (&&) , (||) , (.) , ($) , (<$>)
                                        , sum, remainder, elem, any
                                        )
import      PlutusTx.AssocMap           ( member, lookup )
import      Plutus.V2.Ledger.Contexts   ( findOwnInput )
import      Plutus.V2.Ledger.Api        ( BuiltinData, Validator, mkValidatorScript
                                        , ScriptContext ( scriptContextTxInfo                  )
                                        , TxInfo        ( txInfoInputs                         )
                                        , TxInInfo      ( txInInfoResolved                     )
                                        , TxOut         ( txOutValue, txOutDatum, txOutAddress )
                                        , Value         ( getValue  )
                                        , TokenName     ( TokenName )
                                        )
import      Utilities                   ( wrapValidator )
import      HelperFunctions             ( hashBetInfo, getMagicNumbers, filterByAddress, unsafeFromOutputDatum )
import      DataTypes                   ( RedeemingInfo (..) , BetInfo (BetInfo) )
import      Variables                   ( betNFTCurrencySymbol )

--------------------------------------------------------------------------------------------------------------------------- |
---------------------------------------------------- | On-Chain Code | ---------------------------------------------------- |
{-# INLINABLE wrappedValidator          #-}
{-# INLINABLE typedValidator            #-}
{-# INLINABLE calculateWinConditions    #-}

typedValidator :: RedeemingInfo -> BetInfo -> ScriptContext ->  Bool
typedValidator    redeemingInfo    betInfo    scriptContext  =  bettingNFTPresent                           -- // Checks the integrity of the Bet Information
                                                            &&  (   betInfo == winConditions                -- // Checks if Bet Information match winning conditions of current UTXO
                                                                ||  betInfo >  winConditions && winExists   -- // Checks if Bet Information match winning conditions of one of the rest
                                                                )
    where
        txInfo              = scriptContextTxInfo scriptContext

        currentSCAddress    = case findOwnInput scriptContext of Just txInInfo -> txOutAddress . txInInfoResolved $ txInInfo

        bettingNFTPresent   = any hasNFT $ getValue . txOutValue . txInInfoResolved <$> txInfoInputs txInfo
        hasNFT value        = case lookup betNFTCurrencySymbol value of Just tokens -> TokenName betInfoHash `member` tokens
                                                                        Nothing     -> False
        betInfoHash         = hashBetInfo betInfo

        winConditions       = calculateWinConditions redeemingInfo

        winExists           = betInfo `elem` allWinConditions
        allWinConditions    = calculateWinConditions . unsafeFromOutputDatum . txOutDatum <$> redeemingUTXOs
        redeemingUTXOs      = filterByAddress currentSCAddress (txInInfoResolved <$> txInfoInputs txInfo)
-- 
calculateWinConditions :: RedeemingInfo -> BetInfo
calculateWinConditions    redeemingInfo  = BetInfo winningBet gameNFTReference sessionNumber
            where
                winningBet          = remainder randomNumber 10
                randomNumber        = sum . getMagicNumbers $ getRandomNumber redeemingInfo
                gameNFTReference    = getGameNFTReference redeemingInfo
                sessionNumber       = getSessionNumber'' redeemingInfo

--------------------------------------------------------------------------------------------------------------------------- |
----------------------------------------------- | Compiling Plutus Script | ----------------------------------------------- |

wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator = wrapValidator typedValidator

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

--------------------------------------------------------------------------------------------------------------------------- |