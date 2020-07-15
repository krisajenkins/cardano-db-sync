{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.StateQuery
  (
  -- Export to keep the compiler happy.
    mainnetCardanoLocalNodeConnectInfo
  , queryLocalTip
  , queryHistoryInterpreter
  , getInterpreter

  , StateQueryTMVar (..)
  , newStateQueryTMVar
  , localStateQueryHandler
  ) where

import           Cardano.Api.Typed (CardanoMode, LocalNodeConnectInfo (..), NetworkId (Mainnet),
                    NodeConsensusMode (..), queryNodeLocalState)
import           Cardano.Api.LocalChainSync (getLocalTip)

import           Cardano.BM.Trace (Trace)

import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

-- import qualified Cardano.Db as DB
import           Cardano.DbSync.Types

import           Cardano.Prelude

import           Data.Time.Clock (UTCTime, addUTCTime)

import           Network.TypedProtocol.Core (Peer)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), SystemStart (..))
import           Ouroboros.Consensus.Cardano (SecurityParam (..))
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock, CardanoEras, Query (..))
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (QueryHardFork (GetInterpreter))
import           Ouroboros.Consensus.HardFork.History.Qry (Qry (..), Interpreter)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Ouroboros.Network.Block (Point (..), Tip, getTipPoint)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure, LocalStateQuery)


-- import           Prelude (String)
-- import qualified Prelude

-- import qualified Shelley.Spec.Ledger.Genesis as Shelley

data StateQueryTMVar = StateQueryTMVar

newStateQueryTMVar :: IO StateQueryTMVar
newStateQueryTMVar = panic "Cardano.DbSync.StateQuery.newStateQueryTMVar"

localStateQueryHandler
    :: Trace IO Text -> StateQueryTMVar
    -> Peer (LocalStateQuery blk (Query blk)) pr0 st0 IO ()
localStateQueryHandler = panic "Cardano.DbSync.StateQuery.localStateQueryHandler"

-- -------------------------------------------------------------------------------------------------

getInterpreter :: SocketPath -> IO (Either AcquireFailure (Interpreter (CardanoEras TPraosStandardCrypto)))
getInterpreter spath = do
  let connInfo = mainnetCardanoLocalNodeConnectInfo spath
  point <- getTipPoint <$> queryLocalTip connInfo
  queryHistoryInterpreter connInfo (point, QueryHardFork GetInterpreter)

relToUTCTime :: SystemStart -> RelativeTime -> UTCTime
relToUTCTime (SystemStart start) (RelativeTime rel) = addUTCTime rel start

_slotToTimeEpoch :: SystemStart -> SlotNo -> Qry (UTCTime, EpochNo)
_slotToTimeEpoch start absSlot = do
    relSlot <- QAbsToRelSlot absSlot
    relTime <- QRelSlotToTime relSlot
    utcTime <- relToUTCTime start <$> QRelToAbsTime relTime
    epochSlot <- QRelSlotToEpoch relSlot
    absEpoch  <- QRelToAbsEpoch  epochSlot
    pure (utcTime, absEpoch)

-- -------------------------------------------------------------------------------------------------

mainnetCardanoLocalNodeConnectInfo
    :: SocketPath
    -> LocalNodeConnectInfo CardanoMode (CardanoBlock TPraosStandardCrypto)
mainnetCardanoLocalNodeConnectInfo (SocketPath path) =
  LocalNodeConnectInfo
    { localNodeSocketPath = path
    , localNodeNetworkId = Mainnet
    , localNodeConsensusMode = CardanoMode (EpochSlots 21600) (SecurityParam 10)
    }

queryLocalTip
    :: LocalNodeConnectInfo CardanoMode (CardanoBlock TPraosStandardCrypto)
    -> IO (Tip (CardanoBlock TPraosStandardCrypto))
queryLocalTip = getLocalTip

queryHistoryInterpreter
    :: LocalNodeConnectInfo CardanoMode (CardanoBlock TPraosStandardCrypto)
    -> ( Point (CardanoBlock TPraosStandardCrypto)
       , Query (CardanoBlock TPraosStandardCrypto) (Interpreter (CardanoEras TPraosStandardCrypto))
       )
    -> IO (Either AcquireFailure (Interpreter (CardanoEras TPraosStandardCrypto)))
queryHistoryInterpreter = queryNodeLocalState




{-

--TODO: change this query to be just a protocol client handler to be used with
-- connectToLocalNode. This would involve changing connectToLocalNode to be
-- able to return protocol handler results properly.

-- | Establish a connection to a node and execute a single query using the
-- local state query protocol.
--
queryNodeLocalState
    :: forall mode block result. (Typeable block, Typeable (ApplyTxErr block))
    => LocalNodeConnectInfo mode block -> (Point block, Query block result)
    -> IO (Either AcquireFailure result)
queryNodeLocalState connctInfo pointAndQuery = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
        connctInfo
        nullLocalNodeClientProtocols
            { localStateQueryClient =
                Just (localStateQuerySingle resultVar pointAndQuery)
            }
    atomically (takeTMVar resultVar)
  where
    localStateQuerySingle
        :: TMVar (Either AcquireFailure result)
        -> (Point block, Query block result)
        -> LocalStateQueryClient block (Query block) IO ()
    localStateQuerySingle resultVar (point, query) =
      LocalStateQueryClient $ pure $
        SendMsgAcquire point $
          ClientStAcquiring
            { recvMsgAcquired =
                SendMsgQuery query $
                  ClientStQuerying
                    { recvMsgResult = \result -> do
                        --TODO: return the result via the SendMsgDone rather than
                        -- writing into an mvar
                        atomically $ putTMVar resultVar (Right result)
                        pure $ SendMsgRelease $ StateQuery.SendMsgDone ()
                    }
            , recvMsgFailure = \failure -> do
                --TODO: return the result via the SendMsgDone rather than
                -- writing into an mvar
                atomically $ putTMVar resultVar (Left failure)
                pure $ StateQuery.SendMsgDone ()
            }

-}
