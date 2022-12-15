{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Composed (
    MessageOrLimit(..)

  , mkCardanoTracer
  , mkCardanoTracer'
  , mkDataPointTracer
  , mkMetricsTracer

  , documentCardanoTracer
  ) where

import           Control.Exception (SomeException, catch)
import           Data.Aeson.Types (ToJSON)
import           Data.Maybe (fromMaybe)
import           Data.Text

import           Trace.Forward.Utils.DataPoint (DataPoint (..))

import           Cardano.Logging.Configuration
import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Formatter
import           Cardano.Logging.FrequencyLimiter (LimitingMessage (..))
import           Cardano.Logging.Trace
import           Cardano.Logging.Types

import qualified Control.Tracer as NT
import qualified Data.List as L

data MessageOrLimit m = Message m | Limit LimitingMessage

instance (LogFormatting m) => LogFormatting (MessageOrLimit m) where
  forMachine dtal (Message m) = forMachine dtal m
  forMachine dtal (Limit m)   = forMachine dtal m
  forHuman (Message m) = forHuman m
  forHuman (Limit m)   = forHuman m
  asMetrics (Message m) = asMetrics m
  asMetrics (Limit m)   = asMetrics m

-- | Construct a tracer according to the requirements for cardano node.
--
-- The tracer gets a 'name', which is appended to its namespace.
--
-- The tracer gets a 'namesFor', 'severityFor' and 'privacyFor' function
-- as arguments, to set the logging context accordingly.
--
-- The tracer gets the backends': 'trStdout', 'trForward' and 'mbTrEkg'
-- as arguments.
--
-- The returned tracer need to be configured for the specification of
-- filtering, detailLevel, frequencyLimiting and backends' with formatting before use.
mkCardanoTracer :: forall evt.
     ( LogFormatting evt
     , MetaTrace evt)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> IO (Trace IO evt)
mkCardanoTracer trStdout trForward mbTrEkg tracerPrefix =
    mkCardanoTracer' trStdout trForward mbTrEkg tracerPrefix noHook
  where
    noHook :: Trace IO evt -> IO (Trace IO evt)
    noHook = pure

-- | Adds the possibility to add special tracers via the hook function
mkCardanoTracer' :: forall evt evt1.
     ( LogFormatting evt
     , LogFormatting evt1
     , MetaTrace evt)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> (Trace IO evt1 -> IO (Trace IO evt))
  -> IO (Trace IO evt)
mkCardanoTracer' trStdout trForward mbTrEkg tracerPrefix
  hook = do
    messageTrace     <- withBackendsFromConfig backendsAndFormat
    messageTrace'    <- withLimitersFromConfig
                          (NT.contramap Message messageTrace)
                          (NT.contramap Limit messageTrace)
    messageTrace''   <- hook messageTrace'
    messageTrace'''  <- addContextAndFilter messageTrace''
    messageTrace'''' <- maybeSilent tracerPrefix messageTrace'''

    let metricsTrace = case mbTrEkg of
                          Nothing -> Trace NT.nullTracer
                          Just ekgTrace -> metricsFormatter "Cardano" ekgTrace
    metricsTrace'    <- hook metricsTrace
    let metricsTrace'' = filterTrace
                            (\(_,v) -> not (Prelude.null (asMetrics v)))
                            metricsTrace'

    pure (messageTrace'''' <> metricsTrace'')


  where
    addContextAndFilter :: Trace IO evt -> IO (Trace IO evt)
    addContextAndFilter tr = do
      tr'  <- withDetailsFromConfig tr
      tr'' <- filterSeverityFromConfig tr'
      pure $ withInnerNames
             $ appendPrefixNames tracerPrefix
               $ withSeverity
                 $ withPrivacy
                    $ withDetails
                        tr''

    backendsAndFormat ::
         Maybe [BackendConfig]
      -> Trace m x
      -> IO (Trace IO (MessageOrLimit evt1))
    backendsAndFormat mbBackends _ =
      let backends' = fromMaybe
                      [EKGBackend, Forwarder, Stdout HumanFormatColoured]
                      mbBackends
      in do
        mbForwardTrace <- if Forwarder `L.elem` backends'
                            then fmap (Just . filterTraceByPrivacy (Just Public))
                                  (forwardFormatter Nothing trForward)
                            else pure Nothing
        mbStdoutTrace  <-  if Stdout HumanFormatColoured `L.elem` backends'
                            then fmap Just
                                (humanFormatter True Nothing trStdout)
                            else if Stdout HumanFormatUncoloured `L.elem` backends'
                              then fmap Just
                                  (humanFormatter False Nothing trStdout)
                              else if Stdout MachineFormat `L.elem` backends'
                                then fmap Just
                                  (machineFormatter Nothing trStdout)
                                else pure Nothing
        case mbForwardTrace <> mbStdoutTrace of
          Nothing -> pure $ Trace NT.nullTracer
          Just tr -> pure $ preFormatted backends' tr

-- A simple dataPointTracer which supports building a namespace.
mkDataPointTracer :: forall dp. (ToJSON dp, MetaTrace dp)
  => Trace IO DataPoint
  -> IO (Trace IO dp)
mkDataPointTracer trDataPoint = do
    let tr = NT.contramap DataPoint trDataPoint
    pure $ withInnerNames tr

mkMetricsTracer :: Maybe (Trace IO FormattedMessage) -> Trace IO FormattedMessage
mkMetricsTracer = fromMaybe mempty

documentCardanoTracer ::
     MetaTrace a
  => TraceConfig
  -> Trace IO a
  -> IO [([Text], DocuResult)]
documentCardanoTracer trConfig trace = do
    res <- catch
            (do
              configureTracers trConfig [trace]
              pure True)
            (\(e :: SomeException) -> do
              putStrLn $ "Configuration exception" <> show e
              pure False)
    if res
      then  catch (documentTracer trace)
              (\(e :: SomeException) -> do
                putStrLn $ "Documentation exception" <> show e
                pure [])
      else pure []
