{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import      PlutusTx.Prelude        ( remainder )
import      Plutus.V1.Ledger.Value  ( AssetClass (AssetClass) )
import      Plutus.V2.Ledger.Api    ( CurrencySymbol ( unCurrencySymbol, CurrencySymbol )
                                    , TokenName      ( TokenName     )
                                    , ValidatorHash  ( ValidatorHash )
                                    , POSIXTime      ( POSIXTime     )
                                    )
import      PlutusTx.Builtins       ( blake2b_256, fromBuiltin )
import      PlutusTx.Builtins.Class ( stringToBuiltinByteString, ToBuiltin (toBuiltin) )
import      Data.ByteString.Char8   ( ByteString, putStrLn )
import      Data.Function           ( on )
import      Utilities               ( writeValidatorToFile, writePolicyToFile, writeDataToFile
                                    , bytesFromHex, bytesToHex, posixTimeToIso8601, posixTimeFromIso8601
                                    , validatorHash', currencySymbol
                                    )
import      HelperFunctions         ( getMagicNumbers, hashBetInfo )
import      DataTypes               ( PoolInfo (Pool), BetInfo (BetInfo), RedeemingInfo (Redeeming) )
import      RandomGeneratorSC       qualified
import      BettingSC               qualified
import      RedeemingSC             qualified
import      BetNFTMintingPolicy     qualified

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------------- IO Functions ------------------------------------------------------ |

main :: IO ()
main = do

    writeRandomGeneratorSC
    writeBettingSC
    writeRedeemingSC
    writeBetNFTMintingPolicy

    let info =          "ID :"
            <>  "\n" <> "-- RandomGeneratorSC       : " <> show randomGeneratorSCValidatorHash
            <>  "\n" <> "-- BettingSC               : " <> show bettingSCValidatorHash
            <>  "\n" <> "-- RedeemingSC             : " <> show redeemingSCValidatorHash
            <>  "\n" <> "-- BetNFTMintingPolicy     : " <> show betNFTCurrencySymbol
            <>  "\n" <> "--------------------------------------------------------------------------------------"
            <>  "\n" <> "Magic Numbers :"
            <>  "\n" <> "-- RandomGeneratorSC       : " <> show randomGeneratorSCMagicNumbers
            <>  "\n" <> "-- RedeemingSC             : " <> show redeemingSCMagicNumbers
            <>  "\n" <> "-- BetNFTMintingPolicy     : " <> show betNFTMintingPolicyMagicNumbers

    writeFile "Main.info" info
    writeUnit
    writeInitialHash
--------------------------------------------------------------------------------------------------------------------------- |
writeRandomGeneratorSC, writeBettingSC, writeRedeemingSC, writeBetNFTMintingPolicy  :: IO ()

writeRandomGeneratorSC          =   writeValidatorToFile    "../../Scripts/RandomGeneratorSC.plutus"       RandomGeneratorSC.validator
writeBettingSC                  =   writeValidatorToFile    "../../Scripts/BettingSC.plutus"               BettingSC.validator
writeRedeemingSC                =   writeValidatorToFile    "../../Scripts/RedeemingSC.plutus"             RedeemingSC.validator
writeBetNFTMintingPolicy        =   writePolicyToFile       "../../Scripts/BetNFTMintingPolicy.plutus"     BetNFTMintingPolicy.policy

--------------------------------------------------------------------------------------------------------------------------- |
randomGeneratorSCValidatorHash, bettingSCValidatorHash, redeemingSCValidatorHash    :: ValidatorHash
betNFTCurrencySymbol                                                                :: CurrencySymbol

randomGeneratorSCValidatorHash  =   validatorHash'          RandomGeneratorSC.validator
bettingSCValidatorHash          =   validatorHash'          BettingSC.validator
redeemingSCValidatorHash        =   validatorHash'          RedeemingSC.validator
betNFTCurrencySymbol            =   currencySymbol          BetNFTMintingPolicy.policy
--------------------------------------------------------------------------------------------------------------------------- |
randomGeneratorSCMagicNumbers, redeemingSCMagicNumbers, betNFTMintingPolicyMagicNumbers :: [Integer]

randomGeneratorSCMagicNumbers   = getMagicNumbers b where ValidatorHash b = randomGeneratorSCValidatorHash
redeemingSCMagicNumbers         = getMagicNumbers b where ValidatorHash b = redeemingSCValidatorHash
betNFTMintingPolicyMagicNumbers = getMagicNumbers . unCurrencySymbol      $ betNFTCurrencySymbol
--------------------------------------------------------------------------------------------------------------------------- |
writeUnit :: IO ()
writeUnit = writeDataToFile "../../Data/Unit.json" ()
-- 
writeInitialHash :: IO ()
writeInitialHash = do
    let hash = blake2b_256 $ stringToBuiltinByteString "Some Random Text."
    writeDataToFile "../../Data/InitialHash.json" hash
-- 
makeNewHash :: ByteString -> ByteString -> IO ()
makeNewHash = ((writeDataToFile "../../Data/NewHash.json" . blake2b_256) .) . on (<>) (toBuiltin . bytesFromHex)
-- 
makePoolInfo :: Integer -> Integer -> ByteString -> ByteString -> Integer -> IO ()
makePoolInfo milliseconds1 milliseconds2 b1 b2 sessionNumber = do
    let deadline            = POSIXTime milliseconds1
    let lockingPeriod       = POSIXTime milliseconds2
    let nftCurrencySymbol   = CurrencySymbol $ toBuiltin $ bytesFromHex b1
    let nftTokenName        = TokenName      $ toBuiltin $ bytesFromHex b2
    let randomNFTReference  = AssetClass ( nftCurrencySymbol , nftTokenName )
    let poolInfo            = Pool deadline lockingPeriod randomNFTReference sessionNumber
    whiteLine ; writeDataToFile "../../Data/PoolInfo.json" poolInfo
    whiteLine
-- 
makeBet :: Integer -> ByteString -> ByteString -> Integer -> IO ()
makeBet bet b1 b2 sessionNumber = do
    let nftCurrencySymbol   = CurrencySymbol $ toBuiltin $ bytesFromHex b1
    let nftTokenName        = TokenName      $ toBuiltin $ bytesFromHex b2
    let gameNFTReference    = AssetClass ( nftCurrencySymbol , nftTokenName )
    let betInfo             = BetInfo bet gameNFTReference sessionNumber
    let betInfoHash         = bytesToHex . fromBuiltin $ hashBetInfo betInfo
    let filePath            = "../../Data/BetInfo_" ++ (tail.init.show $ betInfoHash) ++ ".json"
    whiteLine ; writeDataToFile filePath betInfo
    emptyLine ; dashedLine
    putStr "Bet Info Hash : "
    Data.ByteString.Char8.putStrLn betInfoHash
    whiteLine
-- 
makeRedeemingInfo :: ByteString -> ByteString -> ByteString -> Integer -> IO ()
makeRedeemingInfo bHash b1 b2 sessionNumber = do
    let randomHash          = toBuiltin $ bytesFromHex bHash
    let nftCurrencySymbol   = CurrencySymbol $ toBuiltin $ bytesFromHex b1
    let nftTokenName        = TokenName      $ toBuiltin $ bytesFromHex b2
    let gameNFTReference    = AssetClass ( nftCurrencySymbol , nftTokenName )
    let redeemingInfo       = Redeeming randomHash gameNFTReference sessionNumber
    whiteLine ; writeDataToFile "../../Data/RedeemingInfo.json" redeemingInfo
    whiteLine
-- 
getWinningNumber :: ByteString -> IO ()
getWinningNumber = print . flip remainder 10 . randomNumber
    where
        randomNumber = sum . getMagicNumbers . toBuiltin . bytesFromHex
-- 
getPosixTimeFromIso8601 :: String -> POSIXTime
getPosixTimeFromIso8601 str = case posixTimeFromIso8601 str of
    Just t  -> t
    Nothing -> error "Invalid Input!"
-- 
getPoolTimeline :: Integer -> Integer -> IO ()
getPoolTimeline t dt = do
    let deadline = posixTimeToIso8601 (POSIXTime t)
    let maturity = posixTimeToIso8601 (POSIXTime $ t + dt)
    putStr "Deadline : " ; print deadline
    putStr "Maturity : " ; print maturity

---------------------
-- Aesthetic Tools --

emptyLine :: IO ()
emptyLine = Prelude.putStrLn ""
-- 
dashedLine :: IO ()
dashedLine = Prelude.putStrLn "--------------------------------------------------------------------------------"
-- 
whiteLine :: IO ()
whiteLine = Prelude.putStrLn "============================================================================================================"
-- 
--------------------------------------------------------------------------------------------------------------------------- |