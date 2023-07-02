{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns    #-}

module HelperFunctions where

import      PlutusTx.Builtins       ( blake2b_256, consByteString, emptyByteString, indexByteString )
import      PlutusTx.Prelude        ( Integer, BuiltinByteString, Bool
                                    , Eq ((==)) , mconcat, (.) , ($) 
                                    , fst, snd, filter, foldr
                                    )
import      Plutus.V1.Ledger.Value  ( AssetClass (unAssetClass), valueOf )
import      Plutus.V2.Ledger.Api    ( UnsafeFromData ( unsafeFromBuiltinData            )
                                    , TxOut          ( txOutAddress, txOutValue         ) , Address
                                    , OutputDatum    ( OutputDatum                      )
                                    , Datum          ( getDatum                         )
                                    , CurrencySymbol ( unCurrencySymbol, CurrencySymbol )
                                    , TokenName      ( unTokenName                      )
                                    , ValidatorHash  ( ValidatorHash                    )
                                    )
import      DataTypes               ( BetInfo (..) )

--------------------------------------------------------------------------------------------------------------------------- |
{-# INLINABLE filterByAddress           #-}
{-# INLINABLE filterByNFT               #-}
{-# INLINABLE hashBetInfo               #-}
{-# INLINABLE getMagicNumbers           #-}
{-# INLINABLE consValidatorHash         #-}
{-# INLINABLE consCurrencySymbol        #-}
{-# INLINABLE unsafeFromOutputDatum     #-}
-- 
filterByAddress :: Address -> [TxOut] -> [TxOut]
filterByAddress addr = filter containAddr
    where
        containAddr :: TxOut -> Bool
        containAddr = (== addr) . txOutAddress
-- 
filterByNFT :: AssetClass -> [TxOut] -> [TxOut]
filterByNFT nft = filter hasNFT
    where
        hasNFT txOut   = 1 == valueOf (txOutValue txOut) symbol name
        (symbol, name) = unAssetClass nft
-- 
hashBetInfo :: BetInfo -> BuiltinByteString
hashBetInfo betInfo = blake2b_256 $
    consByteString ( getPlayer'sBet    betInfo ) .
    consByteString ( getSessionNumber' betInfo ) .
    mconcat $ [ unCurrencySymbol . fst . unAssetClass $ getGameNFTReference' betInfo
              , unTokenName      . snd . unAssetClass $ getGameNFTReference' betInfo
              ]
-- 
getMagicNumbers :: BuiltinByteString -> [Integer]
getMagicNumbers bltBS = [ indexByteString bltBS i
                        | i <- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27] ]
-- 
consValidatorHash   :: [Integer] -> ValidatorHash
consValidatorHash   = ValidatorHash  . foldr consByteString emptyByteString
-- 
consCurrencySymbol  :: [Integer] -> CurrencySymbol
consCurrencySymbol  = CurrencySymbol . foldr consByteString emptyByteString
-- 
unsafeFromOutputDatum :: UnsafeFromData b => OutputDatum -> b
unsafeFromOutputDatum (OutputDatum datum) = unsafeFromBuiltinData $ getDatum datum
--------------------------------------------------------------------------------------------------------------------------- |