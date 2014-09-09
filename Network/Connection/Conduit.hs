{-# LANGUAGE RankNTypes #-}
-- | A @conduit@ source and sink based on "Network.Connection" from the
-- @connection@ package, and ResourceT aware constructors.
module Network.Connection.Conduit
    ( -- * Source and sink
      sourceConnection
    , sinkConnection
      -- * ResourceT aware constructors
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

-- | Stream the data from the connection. This does not close the connection
-- on completion
sourceConnection :: MonadIO m => C.Connection -> Producer m ByteString
sourceConnection connection =
    loop
  where
    loop = do
        bs <- lift $ liftIO $ C.connectionGetChunk connection
        unless (BS.null bs) $
            yield bs >> loop

-- | Stream all incoming data to the connection. This does not close the
-- connection on completion.
sinkConnection :: MonadIO m => C.Connection -> Consumer ByteString m ()
sinkConnection connection =
    loop
  where
    loop = await >>= maybe (return ()) (\bs -> lift (liftIO $ C.connectionPut connection bs) >> loop)

-- | Create a new connection from a handle. See 'Network.Connection.connectFromHandle'.
connectFromHandle :: (MonadResource m) => C.ConnectionContext -> Handle -> C.ConnectionParams -> m C.Connection
connectFromHandle ctx h params = do
    (_, c) <- liftResourceT $ allocate (C.connectFromHandle ctx h params) C.connectionClose
    return c

-- | Create a new connection. See 'Network.Connection.connectTo'.
connectTo :: (MonadResource m) => C.ConnectionContext -> C.ConnectionParams -> m C.Connection
connectTo ctx params = do
    (_, c) <- liftResourceT $ allocate (C.connectTo ctx params) C.connectionClose
    return c

