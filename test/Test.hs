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

module Main where

import           Control.Monad              hiding (fmap)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO)
import           Wallet.Emulator.Wallet
import           Onchain.RatingToken
import           Onchain.RatingValidatorScript
import           Offchain.Offchain

main :: IO ()
main = runEmulatorTraceIO $ do
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
