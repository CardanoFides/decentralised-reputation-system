{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
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

{-# INLINABLE mkValidator #-}
-- Check if a native token is minted and send to the script address
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx = 
    traceIfFalse "Output does not have correct ADA value" correctAdaValue &&
    traceIfFalse "Output does not have correct Token value" correctMintedToken
  where
    correctAdaValue :: Bool
    correctAdaValue = 
        lovelaces (txOutValue ownOutput) == 10_000_000

    correctMintedToken :: Bool
    correctMintedToken = case flattenValue (txInfoMint info) of
        [(cs, _, _)] -> if cs == ratingTokenSymbol then True else False
        _ -> False

    ratingTokenSymbol :: CurrencySymbol
    ratingTokenSymbol = scriptCurrencySymbol policy

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "Expect exactly one validator script output"

    info :: TxInfo
    info = scriptContextTxInfo ctx

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = ()

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

minLovelace :: Integer
minLovelace = 2_000_000

data StartParams = StartParams
    { spPlaceHolder :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

data PayParams = PayParams
    { ppTokenName :: !TokenName
    , ppAmount :: !Integer
    , ppAddress :: !Address
    } deriving (Generic, ToJSON, FromJSON)

type ReputationSchema =
        Endpoint "start" StartParams
    .\/ Endpoint "pay" PayParams

start :: StartParams -> Contract w ReputationSchema Text ()
start _ = do
    let d = ()
        v = Ada.lovelaceValueOf 10_000_000
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints typedValidator tx    
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "started reputation validator script"

pay :: PayParams -> Contract w ReputationSchema Text ()
pay pp = do
    (rORef, ro) <- findReputationOutput
    Contract.logInfo @String $ printf "found reputation utxo"
    (wORef, wo) <- findWalletOutput $ ppAddress pp
    Contract.logInfo @String $ printf "found wallet utxo"
    pkh <- Contract.ownPaymentPubKeyHash
    Contract.logInfo @String $ printf "PubKeyHash is found"
    let mintVal = Value.singleton curSymbol (ppTokenName pp) (ppAmount pp)
        inputVal = _ciTxOutValue ro
        walletVal = mintVal <> Ada.lovelaceValueOf minLovelace
        d = ()
        r = Redeemer $ PlutusTx.toBuiltinData ()
        lookups = Constraints.mintingPolicy policy <>
                  Constraints.unspentOutputs (Map.singleton rORef ro) <>
                  Constraints.unspentOutputs (Map.singleton wORef wo) <>
                  Constraints.otherScript validator <>
                  Constraints.typedValidatorLookups typedValidator
        tx = Constraints.mustMintValue mintVal <>
             Constraints.mustPayToTheScript d inputVal <>
             Constraints.mustPayToPubKey pkh walletVal <>
             Constraints.mustSpendScriptOutput rORef r <>
             Constraints.mustSpendPubKeyOutput wORef 
    Contract.logInfo @String $ printf "Ready to submit Tx"
    ledgerTx <- submitTxConstraintsWith @Typed lookups tx
    Contract.logInfo @String $ printf "Tx has been submited"
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Paid to reputation validator script"

findReputationOutput :: Contract w s Text (TxOutRef, ChainIndexTxOut)
findReputationOutput = do
    utxos <- utxosAt scrAddress
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [(oref, o)] -> return (oref, o)
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
        { spPlaceHolder = 1
        }
    void $ Emulator.waitNSlots 1

    callEndpoint @"pay" h2 $ PayParams
        { ppTokenName = "TRUST"
        , ppAmount = 1
        , ppAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
