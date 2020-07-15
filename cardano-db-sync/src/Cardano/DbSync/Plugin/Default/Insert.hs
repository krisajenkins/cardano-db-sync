{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Plugin.Default.Insert
  ( insertCardanoBlock
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace)

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)

import           Cardano.DbSync.Error
import qualified Cardano.DbSync.Plugin.Default.Byron.Insert as Byron
import qualified Cardano.DbSync.Plugin.Default.Shelley.Insert as Shelley
import           Cardano.DbSync.Types

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
-- import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
-- import           Ouroboros.Network.Block (Point (..), Tip (..))

insertCardanoBlock
    :: Trace IO Text -> DbSyncEnv -> CardanoBlockTip
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertCardanoBlock tracer env blkTip = do
  case blkTip of
    ByronBlockTip blk tip ->
      Byron.insertByronBlock tracer blk tip
    ShelleyBlockTip blk tip ->
      Shelley.insertShelleyBlock tracer env blk tip
    CardanoBlockTip cblk _tip ->
      case cblk of
        BlockByron (ByronBlock _blk _slot _hash) ->
          panic "insertCardanoBlock: BlockByron"
          -- Byron.insertByronBlock tracer blk (panic "insertCardanoBlock: Cannot provide a Tip")
        BlockShelley _blk ->
          panic "insertCardanoBlock: BlockShelley"
          -- Shelley.insertShelleyBlock tracer env blk (panic "insertCardanoBlock: Cannot provide a Tip")
