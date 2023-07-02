{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Variables where

import      Plutus.V2.Ledger.Api    ( ValidatorHash, CurrencySymbol         )
import      HelperFunctions         ( consValidatorHash, consCurrencySymbol )

--------------------------------------------------------------------------------------------------------------------------- |
{-# INLINABLE randomGeneratorValidatorHash  #-}
{-# INLINABLE redeemingValidatorHash        #-}
{-# INLINABLE betNFTCurrencySymbol          #-}
-- 
randomGeneratorValidatorHash, redeemingValidatorHash    :: ValidatorHash
betNFTCurrencySymbol                                    :: CurrencySymbol
-- 
randomGeneratorValidatorHash    = consValidatorHash     [82,50,166,109,36,83,60,224,151,60,213,175,49,125,18,53,104,126,34,165,132,223,53,50,193,159,49,81]
redeemingValidatorHash          = consValidatorHash     [5,251,60,170,38,121,21,165,4,96,223,211,2,111,147,119,48,97,52,10,224,126,217,27,201,52,75,218]
betNFTCurrencySymbol            = consCurrencySymbol    [112,36,207,199,146,239,153,218,124,211,183,231,114,184,216,152,166,137,251,185,160,14,111,224,230,26,238,20]
--------------------------------------------------------------------------------------------------------------------------- |