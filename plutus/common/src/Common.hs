module Common where

import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.ProtocolVersions as Plutus
import qualified PlutusCore as Plutus

import qualified Data.ByteString.Short as SBS

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let nargs = length args
--   let scriptnum = if nargs > 0 then read (args!!0) else 42
--   let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
--   putStrLn $ "Writing output to: " ++ scriptname
--   writePlutusScript scriptnum scriptname alwaysSucceedsScript alwaysSucceedsScriptShortBs

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just costModel ->
          case Plutus.mkEvaluationContext costModel of
            Right m ->
              let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
                  (logout, e) = Plutus.evaluateScriptCounting Plutus.alonzoPV Plutus.Verbose m scriptSBS [pData]
              in do print ("Log output" :: String) >> print logout
                    case e of
                      Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                      Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
            Left err -> error $ "mkEvaluationContext failed: " <> show err
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
