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
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import qualified Ledger.Typed.Scripts       as Scripts

{-# INLINABLE mkPolicy #-}
mkPolicy :: Ledger.ValidatorHash -> () -> ScriptContext -> Bool
mkPolicy h () ctx =
    traceIfFalse "No reputation validator script present" reputationContractPresent
  where
    reputationContractPresent :: Bool
    reputationContractPresent =
        let xs = [ i
                | i <- txInfoInputs info
                , (addressCredential . txOutAddress . txInInfoResolved) i == Credential.ScriptCredential h
                ]
        in case xs of
            [_] -> True
            _ -> False

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