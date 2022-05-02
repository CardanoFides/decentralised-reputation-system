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

module ReputationToken where

import           Control.Monad              hiding (fmap)
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Map                   as Map
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified Plutus.V1.Ledger.Scripts   as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import           Ledger.Ada                 as Ada
import           Ledger.Constraints         as Constraints
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Prelude                    (IO, Semigroup (..), String)
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

{-# INLINABLE policy #-}
policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

{-# INLINABLE curSymbol #-}
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

data RatingDatum = RatingDatum
    { rdRatingTokenSymbol :: !CurrencySymbol
    , rdRatingTokenName :: !TokenName
    , rdOwner :: !PaymentPubKeyHash
    }

PlutusTx.unstableMakeIsData ''RatingDatum

data Pay = Pay
    { pPayer :: !PaymentPubKeyHash
    , pPay :: !Integer
    }

PlutusTx.unstableMakeIsData ''Pay

data RatingAction = MkPayment Pay

PlutusTx.unstableMakeIsData ''RatingAction

{-# INLINABLE mkValidator #-}
-- Check if a native token is minted and send to the script address
mkValidator :: RatingDatum -> RatingAction -> ScriptContext -> Bool
mkValidator rd (MkPayment Pay {..}) ctx =
    traceIfFalse "Output ADA value is incorrect" correctLockedAda &&
    traceIfFalse "Owner does not receive correct amount of ADA" correctAdaToOwner &&
    traceIfFalse "Output does not have correct Token value" correctMintedToken &&
    traceIfFalse "Output datum hash is incorrect" correctDatumHash
  where
    correctLockedAda :: Bool
    correctLockedAda =
        txOutValue ownOutput == txOutValue ownInput

    correctAdaToOwner :: Bool
    correctAdaToOwner = lovelaces (valuePaidTo info $ unPaymentPubKeyHash $ rdOwner rd) == pPay

    correctMintedToken :: Bool
    correctMintedToken = txInfoMint info == ratingToken

    ratingToken :: Value
    ratingToken = Value.singleton (rdRatingTokenSymbol rd) (rdRatingTokenName rd) 1

    correctDatumHash :: Bool
    correctDatumHash = txOutDatumHash ownOutput == txOutDatumHash ownInput

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "Rating input missing"
        Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "Expect exactly one validator script output"

    info :: TxInfo
    info = scriptContextTxInfo ctx

data Rating
instance Scripts.ValidatorTypes Rating where
    type instance DatumType Rating = RatingDatum
    type instance RedeemerType Rating = RatingAction

typedValidator :: Scripts.TypedValidator Rating
typedValidator = Scripts.mkTypedValidator @Rating
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RatingDatum @RatingAction

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

minLovelace :: Integer
minLovelace = 2_000_000

data StartParams = StartParams
    { spRatingTokenSymbol :: !CurrencySymbol
    , spRatingTokenName :: !TokenName
    } deriving (Generic, ToJSON, FromJSON)

data PayParams = PayParams
    { ppRatingTokenSymbol :: !CurrencySymbol
--    , ppReputationOwner :: !PaymentPubKeyHash
    , ppRatingTokenName :: !TokenName
    , ppTokenAmount :: !Integer
    , ppPayment :: !Integer
    , ppAddress :: !Address
    } deriving (Generic, ToJSON, FromJSON)

type ReputationSchema =
        Endpoint "start" StartParams
    .\/ Endpoint "pay" PayParams

start :: StartParams -> Contract w ReputationSchema Text ()
start sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let d :: RatingDatum
        d = RatingDatum
            { rdRatingTokenSymbol = spRatingTokenSymbol sp
            , rdRatingTokenName = spRatingTokenName sp
            , rdOwner = pkh
            }
        v = Ada.lovelaceValueOf 20_000_000
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints typedValidator tx    
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "started reputation validator script"

pay :: PayParams -> Contract w ReputationSchema Text ()
pay pp = do
    (rORef, ro, rDat) <- findReputationOutput
    Contract.logInfo @String $ printf "found reputation utxo"
    (wORef, wo) <- findWalletOutput $ ppAddress pp
    Contract.logInfo @String $ printf "found wallet utxo"
    pkh <- Contract.ownPaymentPubKeyHash
    Contract.logInfo @String $ printf "PubKeyHash is found"
    let mintVal = Value.singleton curSymbol (ppRatingTokenName pp) (ppTokenAmount pp)
        scriptInputVal = _ciTxOutValue ro
        walletReceivingVal = mintVal <> Ada.lovelaceValueOf minLovelace
        walletPayVal = lovelaceValueOf $ ppPayment pp
        rOwnerPkh = rdOwner rDat
        p = Pay
            { pPayer = pkh
            , pPay = lovelaces walletPayVal
            }
        d = RatingDatum
            { rdRatingTokenSymbol = ppRatingTokenSymbol pp
            , rdRatingTokenName = ppRatingTokenName pp
            , rdOwner = rOwnerPkh
            }
        r = Redeemer $ PlutusTx.toBuiltinData $ MkPayment p
        lookups = Constraints.mintingPolicy policy <>
                  Constraints.unspentOutputs (Map.singleton rORef ro) <>
                  Constraints.unspentOutputs (Map.singleton wORef wo) <>
                  Constraints.otherScript validator <>
                  Constraints.typedValidatorLookups typedValidator
        tx = Constraints.mustMintValue mintVal <>
             Constraints.mustPayToTheScript d scriptInputVal <>
             Constraints.mustPayToPubKey rOwnerPkh walletPayVal <>
             Constraints.mustPayToPubKey pkh walletReceivingVal <>
             Constraints.mustSpendScriptOutput rORef r <>
             Constraints.mustSpendPubKeyOutput wORef
    Contract.logInfo @String $ printf "Ready to submit Tx"
    ledgerTx <- submitTxConstraintsWith @Rating lookups tx
    Contract.logInfo @String $ printf "Tx has been submited"
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Paid to reputation validator script"

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
endpoints = awaitPromise (start' `select` pay') >> endpoints
  where
    start' = endpoint @"start" start
    pay' = endpoint @"pay" pay

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
    let w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"start" h1 $ StartParams
        { spRatingTokenSymbol = curSymbol
        , spRatingTokenName = "TRUST"
        }
    void $ Emulator.waitNSlots 1

    callEndpoint @"pay" h2 $ PayParams
        { ppRatingTokenSymbol = curSymbol
        , ppRatingTokenName = "TRUST"
        , ppTokenAmount = 1
        , ppPayment = 50_000_000
        , ppAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
