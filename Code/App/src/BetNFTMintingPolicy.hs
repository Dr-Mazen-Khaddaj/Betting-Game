{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module BetNFTMintingPolicy where

import      PlutusTx                ( BuiltinData, compile )
import      PlutusTx.Prelude        ( Bool, (&&), (==), (.), ($) )
import      Plutus.V2.Ledger.Api    ( MintingPolicy, mkMintingPolicyScript
                                    , ScriptContext ( scriptContextTxInfo , scriptContextPurpose )
                                    , TxInfo        ( txInfoMint, txInfoOutputs )
                                    , TxOut         ( txOutDatum )
                                    , TokenName     ( TokenName  ), singleton
                                    , ScriptPurpose ( Minting    )
                                    )
import      Utilities               ( wrapPolicy )
import      HelperFunctions         ( hashBetInfo, unsafeFromOutputDatum, filterByNFT )
import      DataTypes               ( BetInfo (..) , PoolInfo (getSessionNumber)      )

--------------------------------------------------------------------------------------------------------------------------- |
---------------------------------------------------- | On-Chain Code | ---------------------------------------------------- |
{-# INLINABLE wrappedPolicy #-}
{-# INLINABLE typedPolicy   #-}

typedPolicy :: BetInfo -> ScriptContext ->  Bool
typedPolicy    betInfo    scriptContext  =  mintedValue    ==  betNFT               -- // Verifies the Validity of the Minted NFT (Given BetInfo)
                                        &&  sessionNumber  ==  gameSessionNumber    -- // Verifies the Validity of the Bet Info
    where
        txInfo              = scriptContextTxInfo scriptContext

    {-  Proposed Variables    -}
        mintedValue         = txInfoMint txInfo
        sessionNumber       = getSessionNumber' betInfo

    {-  Calculated Variables  -}
        betNFT              = singleton betNFTCurrencySymbol (TokenName betInfoHash) 1
        Minting betNFTCurrencySymbol = scriptContextPurpose scriptContext
        betInfoHash         = hashBetInfo betInfo

        gameSessionNumber   = getSessionNumber . unsafeFromOutputDatum $ txOutDatum bettingSCUTXO
        [bettingSCUTXO]     = filterByNFT (getGameNFTReference' betInfo) (txInfoOutputs txInfo)

--------------------------------------------------------------------------------------------------------------------------- |
----------------------------------------------- | Compiling Plutus Script | ----------------------------------------------- |

wrappedPolicy :: BuiltinData -> BuiltinData -> ()
wrappedPolicy = wrapPolicy typedPolicy

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrappedPolicy ||])

--------------------------------------------------------------------------------------------------------------------------- |