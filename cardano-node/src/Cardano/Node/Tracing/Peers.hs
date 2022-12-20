{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Tracing.Peers
  ( NodePeers (..)
  , traceNodePeers
  ) where

import           Cardano.Prelude
import           Data.Aeson (FromJSON, ToJSON)
import           Prelude (error)

import           Cardano.Logging

import           Cardano.Node.Tracing.Tracers.Peer (PeerT, ppPeer)

type PeerInfoPP = Text -- The result of 'ppPeer' function.

-- | This type contains an information about current peers of the node.
--   It will be asked by external applications as a DataPoint.
newtype NodePeers = NodePeers [PeerInfoPP]

deriving instance Generic NodePeers

instance ToJSON NodePeers
instance FromJSON NodePeers

instance MetaTrace NodePeers where
  namespaceFor NodePeers {}  =
    Namespace [] ["NodePeers"]
  severityFor  (Namespace _ ["NodePeers"]) _ =
    Info
  severityFor ns _ =
    error ("NodePeers>>severityFor: Unknown namespace " ++ show ns)
  documentFor  (Namespace _ ["NodePeers"]) =
    ""
  documentFor ns =
     error ("NodePeers>>documentFor: Unknown namespace " ++ show ns)
  allNamespaces = [ Namespace [] ["NodePeers"]]

traceNodePeers
  :: Trace IO NodePeers
  -> [PeerT blk]
  -> IO ()
traceNodePeers tr ev = traceWith tr $ NodePeers (map ppPeer ev)
