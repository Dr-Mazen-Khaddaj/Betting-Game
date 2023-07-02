{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns    #-}

module BettingSC where

import      PlutusTx                    ( ToData (toBuiltinData) , compile )
import      PlutusTx.Prelude            ( Bool, Maybe (Just) , Eq ((==)) , (+) , (&&) , (<$>) , ($) , (.) , error )
import      PlutusTx.AssocMap           qualified as Map
import      Plutus.V1.Ledger.Address    ( scriptHashAddress )
import      Plutus.V1.Ledger.Value      ( AssetClass, assetClassValueOf, valueOf, flattenValue, assetClass )
import      Plutus.V1.Ledger.Interval   ( contains )
import      Plutus.V2.Ledger.Contexts   ( findOwnInput )
import      Plutus.V2.Ledger.Api        ( BuiltinData, Validator, mkValidatorScript
                                        , ScriptContext ( scriptContextTxInfo                           )
                                        , TxInfo        ( txInfoInputs, txInfoOutputs, txInfoValidRange )
                                        , TxInInfo      ( txInInfoResolved                              )
                                        , TxOut         ( txOutAddress, txOutValue, txOutDatum          )
                                        , OutputDatum   ( OutputDatum     )
                                        , Datum         ( Datum           )
                                        , Value         ( Value, getValue ), singleton, adaSymbol, adaToken
                                        , to, from
                                        )
import      Utilities                   ( wrapValidator                                        )
import      HelperFunctions             ( filterByAddress, unsafeFromOutputDatum               )
import      Variables                   ( redeemingValidatorHash, randomGeneratorValidatorHash )
import      DataTypes                   ( PoolInfo (..) , RedeemingInfo (..)                   )

--------------------------------------------------------------------------------------------------------------------------- |
---------------------------------------------------- | On-Chain Code | ---------------------------------------------------- |
{-# INLINABLE mkWrappedValidator    #-}
{-# INLINABLE typedValidator        #-}
{-# INLINABLE resetJackpot          #-}
{-# INLINABLE getNFTAssetClass      #-}
{-# INLINABLE bettingAmount         #-}

bettingAmount :: Value
bettingAmount = singleton adaSymbol adaToken 100000000

typedValidator :: PoolInfo -> () -> ScriptContext -> Bool
typedValidator    poolInfo    ()    scriptContext
    | beforeDeadline = outputValue == inputValue + bettingAmount
                    && outputDatum == inputDatum
                    && updatingRandomNumber

    | afterMaturity  = outputValue == resetJackpot inputValue
                    && outputDatum == newSessionDatum
                    && updatingRandomNumber
                    && redeemingValue == jackpot
                    && redeemingDatum == redeemingInfoDatum

    where
        txInfo              = scriptContextTxInfo scriptContext

-- @ BettingSC

        currentSCAddress    = case findOwnInput scriptContext of Just txInInfo -> txOutAddress . txInInfoResolved $ txInInfo
            
        [inputSCUTXO ]      = filterByAddress currentSCAddress $ txInInfoResolved <$> txInfoInputs  txInfo 
        [outputSCUTXO]      = filterByAddress currentSCAddress $                      txInfoOutputs txInfo 

        inputValue          = txOutValue  inputSCUTXO
        outputValue         = txOutValue outputSCUTXO

        inputDatum          = txOutDatum  inputSCUTXO
        outputDatum         = txOutDatum outputSCUTXO

    {-  New Session Datum       -}
        newSessionPoolInfo  = poolInfo
                            { getDeadline       = maturity + getLockingPeriod poolInfo
                            , getSessionNumber  = getSessionNumber poolInfo + 2
                            }
        newSessionDatum     = OutputDatum . Datum . toBuiltinData $ newSessionPoolInfo

    {-  Time-related Variables  -}
        deadline            = getDeadline poolInfo
        maturity            = deadline + getLockingPeriod poolInfo
        beforeDeadline      = to   deadline `contains` txInfoValidRange txInfo
        afterMaturity       = from maturity `contains` txInfoValidRange txInfo

-- @ RandomGeneratorSC

        randomGeneratorAddress  = scriptHashAddress randomGeneratorValidatorHash
        [randomGeneratorUTXO]   = filterByAddress randomGeneratorAddress $ txInInfoResolved <$> txInfoInputs txInfo
        updatingRandomNumber    = (== 1) $ assetClassValueOf (txOutValue randomGeneratorUTXO) (getRandomNFTReference poolInfo)

-- @ RedeemingSC

    {-  Proposed Variables      -}
        redeemingAddress    = scriptHashAddress redeemingValidatorHash
        [redeemingUTXO]     = filterByAddress redeemingAddress $ txInfoOutputs txInfo
        redeemingValue      = txOutValue redeemingUTXO
        redeemingDatum      = txOutDatum redeemingUTXO

    {-  Calculated Variables    -}
        jackpot             = singleton adaSymbol adaToken $ valueOf inputValue adaSymbol adaToken

        redeemingInfo       = Redeeming
                            { getRandomNumber       = unsafeFromOutputDatum $ txOutDatum randomGeneratorUTXO
                            , getGameNFTReference   = getNFTAssetClass inputValue
                            , getSessionNumber''    = getSessionNumber poolInfo
                            }
        redeemingInfoDatum  = OutputDatum . Datum . toBuiltinData $ redeemingInfo
---------------------------------------------------------
resetJackpot :: Value -> Value
resetJackpot = Value . Map.insert adaSymbol (Map.singleton adaToken 100000000) . getValue
-- 
getNFTAssetClass :: Value -> AssetClass
getNFTAssetClass v = case flattenValue . Value . Map.delete adaSymbol . getValue $ v of
    [(symbol , name , n)] -> if n == 1
                                then assetClass symbol name
                                else error ()

--------------------------------------------------------------------------------------------------------------------------- |
----------------------------------------------- | Compiling Plutus Script | ----------------------------------------------- |

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator typedValidator

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkWrappedValidator ||])

--------------------------------------------------------------------------------------------------------------------------- |