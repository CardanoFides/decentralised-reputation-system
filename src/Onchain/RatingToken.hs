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
import           Onchain.RatingValidatorScript (RatingDatum(..))
{-# INLINABLE mkPolicy #-}
mkPolicy :: Ledger.ValidatorHash -> () -> ScriptContext -> Bool
mkPolicy h () ctx =
    traceIfFalse "No validator script present" validatorPresent &&
    traceIfFalse "Wrong token to mint" checkTokenToMint
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

    checkTokenToMint :: Bool
    checkTokenToMint = case flattenValue (txInfoMint info) of
        [(_, tn, amt)] ->
            case validatorDatum of
                Nothing -> traceError "No datum is found"
                Just (Datum d) ->
                    case PlutusTx.fromBuiltinData d of
                        Nothing -> traceError "Error decoding data"
                        Just rd -> tn == rdRatingTokenName rd && amt == 1
        _               -> traceError "Error in token to be minted"

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
