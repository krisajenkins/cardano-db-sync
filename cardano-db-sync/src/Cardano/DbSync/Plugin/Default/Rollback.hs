{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default.Rollback
  ( rollbackToPoint
  , unsafeRollback
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import           Data.Text (Text)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import qualified Cardano.DbSync.Plugin.Default.Byron.Rollback as Byron
import qualified Cardano.DbSync.Plugin.Default.Shelley.Rollback as Shelley
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Network.Block (BlockNo (..), Point (..))
import           Ouroboros.Network.Point as Point

rollbackToPoint :: Trace IO Text -> CardanoPoint -> IO (Either DbSyncNodeError ())
rollbackToPoint trce cpnt =
  case cpnt of
    ByronPoint point ->
      Byron.rollbackToPoint trce point
    ShelleyPoint point ->
      Shelley.rollbackToPoint trce point
    CardanoPoint (Point pnt) ->
      case pnt of
        Origin -> pure $ Right ()
        At (Block slot hash) -> DB.runDbNoLogging $ runExceptT (action slot $ getOneEraHash hash)
  where
    action :: MonadIO m => SlotNo -> ByteString -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action slot hash = do
        liftIO . logInfo trce $
            mconcat
              [ "Shelley: Rolling back to slot ", textShow (unSlotNo slot)
              , ", hash ", renderByteArray hash
              ]
        xs <- lift $ DB.queryBlockNosWithSlotNoGreater (unSlotNo slot)
        liftIO . logInfo trce $
            mconcat
              [ "Shelley: Deleting blocks numbered: ", textShow (map unBlockNo xs)
              ]
        mapM_ (void . lift . DB.deleteCascadeBlockNo) xs


-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
unsafeRollback trce (SlotNo slotNo) = do
  logInfo trce $ "Forced rollback to slot " <> textShow slotNo
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
