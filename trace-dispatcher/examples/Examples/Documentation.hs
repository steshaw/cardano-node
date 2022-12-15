{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Examples.Documentation (
  docTracers
) where

import qualified Data.Text.IO as T

import           Cardano.Logging
import           Examples.TestObjects
import qualified Data.Map as Map

docTracers :: IO ()
docTracers = do
  t <- standardTracer
  t1' <- humanFormatter True Nothing t
  t1  :: Trace IO (TraceForgeEvent LogBlock)
      <- filterSeverityFromConfig
          (appendPrefixName "node1"
            $ withSeverity
              $ withPrivacy
                $ withDetails t1')
  t2' <- machineFormatter Nothing t
  t2  :: Trace IO (TraceForgeEvent LogBlock)
      <- filterSeverityFromConfig
          (appendPrefixName "node2"
            $ withSeverity
              $ withPrivacy
                $ withDetails t2')
  configureTracers config1 [t1, t2]
  bl <- documentTracer [t1, t2]
  res <- docuResultsToText bl config1
  T.writeFile "/tmp/Testdocu.md" res

config1 :: TraceConfig
config1 = TraceConfig {
      tcOptions = Map.fromList
          [ ([], [ConfSeverity (SeverityF Nothing)])
          , (["node2"], [ConfSeverity (SeverityF (Just Info))])
          ]
    , tcForwarder = TraceOptionForwarder {
        tofConnQueueSize = 100
      , tofDisconnQueueSize = 1000
      , tofVerbosity = Minimum
      }
    , tcNodeName = Nothing
    , tcPeerFrequency = Nothing
    , tcResourceFrequency = Nothing
    }