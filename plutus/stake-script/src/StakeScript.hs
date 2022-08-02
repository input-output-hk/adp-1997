{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This example is taken directly from cardano-api, written by Jordan Millar, IOHK

module StakeScript
  ( stakeScriptScript
  , stakeScriptScriptShortBs
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
-- import           Data.String (fromString)

import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Plutus.V1.Ledger.Contexts as Plutus
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import qualified PlutusTx.Trace as Trace
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator
  :: ()
  -- ^ Datum
  -> Integer
  -- ^ Redeemer
  -> Plutus.ScriptContext
  -- ^ Script context
  -> Bool
mkValidator _ _ ctx =
  case Plutus.scriptContextPurpose ctx of
    (Plutus.Minting _)    -> Trace.traceError "minting"
    (Plutus.Spending _)   -> Trace.traceError "spending"
    (Plutus.Rewarding _)  -> Trace.traceError "rewarding"
    (Plutus.Certifying _) -> Trace.traceError "certifying"

data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType Typed = ()
  type instance RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @() @Integer

validator :: Plutus.Validator
validator = Scripts.validatorScript typedValidator

valHash :: Plutus.ValidatorHash
valHash = Scripts.validatorHash typedValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

stakeScriptScriptShortBs :: SBS.ShortByteString
stakeScriptScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

stakeScriptScript :: PlutusScript PlutusScriptV1
stakeScriptScript = PlutusScriptSerialised stakeScriptScriptShortBs
