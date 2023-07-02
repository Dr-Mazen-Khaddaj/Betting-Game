{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns    #-}

module RandomGeneratorSC where

import      PlutusTx                    ( compile )
import      PlutusTx.Builtins           ( blake2b_256 , error )
import      PlutusTx.Prelude            ( BuiltinByteString, Bool, Maybe (Just) , (==) , (&&) , ($) , (.) , (<$>) , (<>) )
import      PlutusTx.AssocMap           qualified as Map
import      Plutus.V1.Ledger.Value      ( valueOf , flattenValue )
import      Plutus.V2.Ledger.Contexts   ( findOwnInput )
import      Plutus.V2.Ledger.Api        ( Validator, mkValidatorScript
                                        , ScriptContext ( scriptContextTxInfo, scriptContextPurpose )
                                        , TxInfo        ( txInfoInputs, txInfoOutputs               )
                                        , TxInInfo      ( txInInfoResolved                          )
                                        , TxOut         ( txOutDatum, txOutAddress, txOutValue      )
                                        , Value         ( Value, getValue ) , singleton, adaSymbol, adaToken
                                        , TxOutRef      ( txOutRefId      ) , TxId ( getTxId )
                                        , ScriptPurpose ( Spending        )
                                        )
import      Utilities                   ( wrapValidator )
import      HelperFunctions             ( filterByAddress, unsafeFromOutputDatum )

--------------------------------------------------------------------------------------------------------------------------- |
---------------------------------------------------- | On-Chain Code | ---------------------------------------------------- |
{-# INLINABLE typedValidator    #-}
{-# INLINABLE getNFT            #-}
{-# INLINABLE shuffle           #-}
--
typedValidator :: BuiltinByteString -> () -> ScriptContext  ->  Bool
typedValidator    oldHash              ()    scriptContext   =  outputLovelace   ==  2000000
                                                            &&  inputNFT         ==  outputNFT
                                                            &&  proposedNewHash  ==  shuffledNewHash
    where
        txInfo              = scriptContextTxInfo scriptContext

        currentSCAddress    = case findOwnInput scriptContext of Just txInInfo -> txOutAddress . txInInfoResolved $ txInInfo

        [ inputSCUTXO]      = filterByAddress currentSCAddress $ txInInfoResolved <$> txInfoInputs  txInfo
        [outputSCUTXO]      = filterByAddress currentSCAddress $                      txInfoOutputs txInfo

        outputLovelace      = valueOf (txOutValue outputSCUTXO) adaSymbol adaToken

        inputNFT            = getNFT $ txOutValue  inputSCUTXO
        outputNFT           = getNFT $ txOutValue outputSCUTXO

        proposedNewHash     = unsafeFromOutputDatum $ txOutDatum outputSCUTXO
        shuffledNewHash     = shuffle scriptContext oldHash
---------------------------------------------------------
getNFT :: Value -> Value
getNFT v = case flattenValue . Value . Map.delete adaSymbol . getValue $ v of
    [(symbol , name , n)] -> if n == 1
                                then singleton symbol name 1
                                else error ()
-- 
shuffle :: ScriptContext -> BuiltinByteString -> BuiltinByteString
shuffle scriptContext oldHash = blake2b_256 (oldHash <> previousTxId)
    where
        previousTxId        = getTxId $ txOutRefId txOutRef
        Spending txOutRef   = scriptContextPurpose scriptContext
--------------------------------------------------------------------------------------------------------------------------- |
----------------------------------------------- | Compiling Plutus Script | ----------------------------------------------- |

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapValidator typedValidator ||])

--------------------------------------------------------------------------------------------------------------------------- |