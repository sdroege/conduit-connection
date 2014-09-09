{-# LANGUAGE RankNTypes #-}
module Network.Connection.Conduit
    ( sourceConnection
    , sinkConnection
    , connectFromHandle
    , connectTo
    ) where

import Data.Conduit
import qualified Network.Connection as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource
import Control.Monad (unless)
import System.IO (Handle)

sourceConnection :: MonadIO m => C.Connection -> Producer m ByteString
sourceConnection connection =
    loop
  where
    loop = do
        bs <- lift $ liftIO $ C.connectionGetChunk connection
        unless (BS.null bs) $
            yield bs >> loop

sinkConnection :: MonadIO m => C.Connection -> Consumer ByteString m ()
sinkConnection connection =
    loop
  where
    loop = await >>= maybe (return ()) (\bs -> lift (liftIO $ C.connectionPut connection bs) >> loop)

connectFromHandle :: (MonadResource m) => C.ConnectionContext -> Handle -> C.ConnectionParams -> m C.Connection
connectFromHandle ctx h params = do
    (_, c) <- liftResourceT $ allocate (C.connectFromHandle ctx h params) C.connectionClose
    return c

connectTo :: (MonadResource m) => C.ConnectionContext -> C.ConnectionParams -> m C.Connection
connectTo ctx params = do
    (_, c) <- liftResourceT $ allocate (C.connectTo ctx params) C.connectionClose
    return c

