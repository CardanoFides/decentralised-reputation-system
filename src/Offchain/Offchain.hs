{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Offchain.Offchain where

import           Control.Monad              hiding (fmap)
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Map                   as Map
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import           Ledger.Ada                 as Ada
import           Ledger.Constraints         as Constraints
import           Ledger.Value               as Value
import           Prelude                    (IO, Semigroup (..), String)
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet
import           Onchain.RatingToken
import           Onchain.RatingValidatorScript

data StartParams = StartParams
    { spRatingTokenSymbol :: !CurrencySymbol
    , spRatingTokenName :: !TokenName
    } deriving (Generic, ToJSON, FromJSON)

data PayParams = PayParams
    { ppPayment :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

data RateParams = RateParams
    { rpScore :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

type ReputationSchema =
        Endpoint "start" StartParams
    .\/ Endpoint "pay" PayParams
    .\/ Endpoint "rate" RateParams

start :: StartParams -> Contract w ReputationSchema Text ()
start sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let d :: RatingDatum
        d = RatingDatum
            { rdRatingTokenSymbol = spRatingTokenSymbol sp
            , rdRatingTokenName = spRatingTokenName sp
            , rdOwner = pkh
            , rdScoreSum = 0
            , rdRatingCount = 0
            }
        v = Ada.lovelaceValueOf 20_000_000
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints typedValidator tx    
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "started reputation validator script"

pay :: PayParams -> Contract w ReputationSchema Text ()
pay pp = do
    (rORef, ro, rDat) <- findReputationOutput
    Contract.logInfo @String $ printf "found reputation utxo for making payment"
    pkh <- Contract.ownPaymentPubKeyHash
    Contract.logInfo @String $ printf "Own PubKeyHash is found"
    let mintVal = Value.singleton (rdRatingTokenSymbol rDat) (rdRatingTokenName rDat) 1
        scriptInputVal = _ciTxOutValue ro
        walletReceivingVal = mintVal <> Ada.toValue minAdaTxOut
        walletPayVal = lovelaceValueOf $ ppPayment pp
        rOwnerPkh = rdOwner rDat
        p = Pay
            { pPayer = pkh
            , pPay = lovelaces walletPayVal
            }
        r = Redeemer $ PlutusTx.toBuiltinData $ MkPayment p
        lookups = Constraints.mintingPolicy (policy valHash) <>
                  Constraints.unspentOutputs (Map.singleton rORef ro) <>
                  Constraints.otherScript validator <>
                  Constraints.typedValidatorLookups typedValidator
        tx = Constraints.mustMintValue mintVal <>
             Constraints.mustPayToTheScript rDat scriptInputVal <>
             Constraints.mustPayToPubKey rOwnerPkh walletPayVal <>
             Constraints.mustPayToPubKey pkh walletReceivingVal <>
             Constraints.mustSpendScriptOutput rORef r
    Contract.logInfo @String $ printf "Ready to submit Tx for making payment"
    ledgerTx <- submitTxConstraintsWith @RatingScript lookups tx
    Contract.logInfo @String $ printf "Tx for making payment has been submited"
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Paid to reputation validator script"

rate :: RateParams -> Contract w ReputationSchema Text ()
rate rp = do
    (rORef, ro, rDat) <- findReputationOutput
    Contract.logInfo @String $ printf "found reputation utxo for providing rating"
    let scriptInputVal = _ciTxOutValue ro
        ratingToken = Value.singleton (rdRatingTokenSymbol rDat) (rdRatingTokenName rDat) 1
        scriptOutputVal = scriptInputVal <> ratingToken
        outputDatum = RatingDatum
            { rdRatingTokenSymbol = rdRatingTokenSymbol rDat
            , rdRatingTokenName = rdRatingTokenName rDat
            , rdOwner = rdOwner rDat
            , rdScoreSum = rdScoreSum rDat + rpScore rp
            , rdRatingCount = rdRatingCount rDat + 1
            }
        r = Rating
            { rScore = rpScore rp
            }
        red = Redeemer $ PlutusTx.toBuiltinData $ PrvdRating r
        lookups = Constraints.unspentOutputs (Map.singleton rORef ro) <>
                  Constraints.otherScript validator <>
                  Constraints.typedValidatorLookups typedValidator
        tx = Constraints.mustPayToTheScript outputDatum scriptOutputVal <>
             Constraints.mustSpendScriptOutput rORef red
    Contract.logInfo @String $ printf "Ready to submit Tx for providing rating"
    ledgerTx <- submitTxConstraintsWith @RatingScript lookups tx
    Contract.logInfo @String $ printf "Tx for providing rating has been submitted"
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Provided rating to reputation validator script"

findReputationOutput :: Contract w s Text (TxOutRef, ChainIndexTxOut, RatingDatum)
findReputationOutput = do
    utxos <- utxosAt scrAddress
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _ -> Contract.throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> Contract.throwError "datum has wrong type"
                Just d -> return (oref, o, d)

        _ -> Contract.throwError "Reputation utxo is wrong"

findWalletOutput :: Address -> Contract w s Text (TxOutRef, ChainIndexTxOut) 
findWalletOutput walletAddress = do
    utxos <- utxosAt walletAddress
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [] -> Contract.throwError "wallet UTxO is not found"
        (oref, o) : _ -> return (oref, o)

endpoints :: Contract () ReputationSchema Text ()
endpoints = awaitPromise (start' `select` pay' `select` rate') >> endpoints
  where
    start' = endpoint @"start" start
    pay' = endpoint @"pay" pay
    rate' = endpoint @"rate" rate

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
    let w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"start" h1 $ StartParams
        { spRatingTokenSymbol = curSymbol valHash
        , spRatingTokenName = "TRUST"
        }
    void $ Emulator.waitNSlots 1

    callEndpoint @"pay" h2 $ PayParams
        { ppPayment = 50_000_000
        }
    void $ Emulator.waitNSlots 1

    callEndpoint @"rate" h2 $ RateParams
        {rpScore = 5
        }
    void $ Emulator.waitNSlots 1
