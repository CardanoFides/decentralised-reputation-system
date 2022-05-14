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

module Onchain.RatingToken where

import qualified Plutus.V1.Ledger.Scripts   as Scripts
import qualified Plutus.V1.Ledger.Credential as Credential
import           Plutus.V1.Ledger.Value     (flattenValue)
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import qualified Ledger.Typed.Scripts       as Scripts

{-# INLINABLE mkPolicy #-}
mkPolicy :: Ledger.ValidatorHash -> () -> ScriptContext -> Bool
mkPolicy h () ctx =
    traceIfFalse "No validator script present" validatorPresent &&
    traceIfFalse "Wrong amount to mint" checkMintedAmount
  where
    validatorPresent :: Bool
    validatorPresent = case validatorDatum of
        Just _ -> True
        _ -> False

    validatorDatum :: Maybe Datum
    validatorDatum =
        let xs = [ i | i <- txInfoInputs info
                 , (addressCredential . txOutAddress . txInInfoResolved) i ==
                   Credential.ScriptCredential h
                 ]
        in case xs of
            [ i ] ->
                let maybeDh = (txOutDatumHash . txInInfoResolved) i
                in case maybeDh of
                    Just dh -> findDatum dh info
                    _ -> Nothing
            _ -> Nothing

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt)] -> amt == 1
        _               -> False

    info :: TxInfo
    info = scriptContextTxInfo ctx

{-# INLINABLE policy #-}
policy :: Ledger.ValidatorHash -> Scripts.MintingPolicy
policy h = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode h

{-# INLINABLE curSymbol #-}
curSymbol :: Ledger.ValidatorHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
