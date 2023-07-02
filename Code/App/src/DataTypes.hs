{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module DataTypes where

import      PlutusTx                ( unstableMakeIsData )
import      PlutusTx.Prelude        ( BuiltinByteString, Integer
                                    , Eq ((==)) , Ord (compare) , (&&)
                                    )
import      PlutusTx.Builtins       ( error      )
import      Plutus.V1.Ledger.Value  ( AssetClass )
import      Plutus.V2.Ledger.Api    ( POSIXTime  )

data PoolInfo = Pool
    { getDeadline           :: POSIXTime
    , getLockingPeriod      :: POSIXTime
    , getRandomNFTReference :: AssetClass
    , getSessionNumber      :: Integer
    }
unstableMakeIsData ''PoolInfo

data RedeemingInfo = Redeeming
    { getRandomNumber       :: BuiltinByteString
    , getGameNFTReference   :: AssetClass
    , getSessionNumber''    :: Integer
    }
unstableMakeIsData ''RedeemingInfo

data BetInfo = BetInfo
    { getPlayer'sBet        :: Integer
    , getGameNFTReference'  :: AssetClass
    , getSessionNumber'     :: Integer
    }
unstableMakeIsData ''BetInfo
-- 
instance Eq BetInfo where
    BetInfo     { getPlayer'sBet = betA , getGameNFTReference' = refA , getSessionNumber' = ssnA } ==
        BetInfo { getPlayer'sBet = betB , getGameNFTReference' = refB , getSessionNumber' = ssnB } =
            betA == betB &&
            refA == refB &&
            ssnA == ssnB
-- 
instance Ord BetInfo where
    BetInfo     { getPlayer'sBet =_betA , getGameNFTReference' = refA , getSessionNumber' = ssnA } `compare`
        BetInfo { getPlayer'sBet =_betB , getGameNFTReference' = refB , getSessionNumber' = ssnB } = if
            refA == refB then ssnA `compare` ssnB
                         else error ()
