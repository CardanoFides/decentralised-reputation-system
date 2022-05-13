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

module Onchain.RatingValidatorScript where
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import           Ledger.Ada                 as Ada
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Prelude                    (Semigroup (..))

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

data RatingDatum = RatingDatum
    { rdRatingTokenSymbol :: !CurrencySymbol
    , rdRatingTokenName :: !TokenName
    , rdOwner :: !PaymentPubKeyHash
    , rdScoreSum :: !Integer
    , rdRatingCount :: !Integer
    }

instance Eq RatingDatum where
    {-# INLIABLE (==) #-}
    a == b = (rdRatingTokenSymbol a == rdRatingTokenSymbol b) &&
             (rdRatingTokenName a == rdRatingTokenName b) &&
             (rdOwner a == rdOwner b) &&
             (rdScoreSum a == rdScoreSum b) &&
             (rdRatingCount a == rdRatingCount b)

PlutusTx.unstableMakeIsData ''RatingDatum

data Pay = Pay
    { pPayer :: !PaymentPubKeyHash
    , pPay :: !Integer
    }

PlutusTx.unstableMakeIsData ''Pay

data Rating = Rating
    { rScore :: !Integer
    }

PlutusTx.unstableMakeIsData ''Rating

data RatingAction = MkPayment Pay | PrvdRating Rating

PlutusTx.unstableMakeIsData ''RatingAction

{-# INLINABLE mkValidator #-}
mkValidator :: RatingDatum -> RatingAction -> ScriptContext -> Bool
mkValidator rd ra ctx = case ra of
    (MkPayment Pay {..}) ->
        traceIfFalse "Output value is incorrect" correctLockedValue &&
        traceIfFalse "Owner does not receive correct amount of ADA" correctAdaToOwner &&
        traceIfFalse "Payer does not receive correct value" correctValueToPayer &&
        traceIfFalse "RatingToken not minted" correctMintedToken &&
        traceIfFalse "Output datum hash is different" sameDatumHash
      where
        correctAdaToOwner :: Bool
        correctAdaToOwner =
            (valuePaidTo info $ unPaymentPubKeyHash $ rdOwner rd) ==
            Ada.lovelaceValueOf pPay

        correctValueToPayer :: Bool
        correctValueToPayer =
            (valuePaidTo info $ unPaymentPubKeyHash pPayer) ==
            valueSpent info  -- total value spent by this tx.
            - txOutValue ownInput  -- value locked by this script
            - (valuePaidTo info $ unPaymentPubKeyHash $ rdOwner rd) -- value to be paid to owner
            - txInfoFee info  -- tx fee
            + ratingToken  -- payer must receive a newly minted rating token

    (PrvdRating Rating {..}) ->
        traceIfFalse "Malformed rating" correctRating &&
        traceIfFalse "Rating Token is not locked" correctLockedRatingToken &&
        traceIfFalse "Output datum is incorrect" correctOutputDatum
      where
        correctRating :: Bool
        correctRating = rScore >= 1 && rScore <= 5

        correctLockedRatingToken :: Bool
        correctLockedRatingToken =
            txOutValue ownOutput == txOutValue ownInput <> ratingToken

        correctOutputDatum :: Bool
        correctOutputDatum =
            (rdRatingTokenSymbol rd == rdRatingTokenSymbol outputDatum) &&
            (rdRatingTokenName rd == rdRatingTokenName outputDatum) &&
            (rdOwner rd == rdOwner outputDatum) &&
            (rdScoreSum rd + rScore  == rdScoreSum outputDatum) &&
            (rdRatingCount rd + 1 == rdRatingCount outputDatum)
  where
    correctLockedValue :: Bool
    correctLockedValue =
        txOutValue ownOutput == txOutValue ownInput

    info :: TxInfo
    info = scriptContextTxInfo ctx

    correctMintedToken :: Bool
    correctMintedToken = txInfoMint info == ratingToken

    ratingToken :: Value
    ratingToken =
        Value.singleton (rdRatingTokenSymbol rd) (rdRatingTokenName rd) 1

    sameDatumHash :: Bool
    sameDatumHash = txOutDatumHash ownOutput == txOutDatumHash ownInput

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "Rating input missing"
        Just i -> txInInfoResolved i

    ownOutput   :: TxOut
    outputDatum :: RatingDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "Expected exactly one validator script output"

data RatingScript
instance Scripts.ValidatorTypes RatingScript where
    type instance DatumType RatingScript = RatingDatum
    type instance RedeemerType RatingScript = RatingAction

{-# INLINABLE typedValidator #-}
typedValidator :: Scripts.TypedValidator RatingScript
typedValidator = Scripts.mkTypedValidator @RatingScript
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RatingDatum @RatingAction

{-# INLINABLE validator #-}
validator :: Validator
validator = Scripts.validatorScript typedValidator

{-# INLINABLE valHash #-}
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

