{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

import Network.Connection

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Network.Connection.Conduit as CCon

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Test.HUnit

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain tests

setupSocket :: IO (Socket, PortNumber)
setupSocket = do
    let addr = SockAddrInet aNY_PORT iNADDR_ANY

    s <- socket AF_INET Stream defaultProtocol
    bind s addr
    listen s 5

    p <- socketPort s

    return (s, p)

testSinkConnection :: Assertion
testSinkConnection = do
    (s, p) <- setupSocket

    let input = ["abcde", "fghij", "klmno"]

    mOutput <- newEmptyMVar

    _ <- forkIO $ do
        (s', _) <- accept s

        let recvAll l = do
                            b <- recv s' 4096
                            if b == B.empty then
                                return l
                            else
                                recvAll (l ++ [b])
        o <- recvAll []
        putMVar mOutput o
        close s'

    runResourceT $ do
        ctx <- liftIO initConnectionContext
        c <- CCon.connectTo ctx (ConnectionParams "localhost" p Nothing Nothing)
        CL.sourceList input $$ CCon.sinkConnection c

    output <- takeMVar mOutput

    B.concat input @=? B.concat output

    close s

testSourceConnection :: Assertion
testSourceConnection = do
    (s, p) <- setupSocket

    let input = ["abcde", "fghij", "klmno"]

    _ <- forkIO $ do
        (s', _) <- accept s
        forM_ input (send s')
        close s'

    output <- runResourceT $ do
        ctx <- liftIO initConnectionContext
        c <- CCon.connectTo ctx (ConnectionParams "localhost" p Nothing Nothing)
        CCon.sourceConnection c $$ CL.consume

    B.concat input @=? B.concat output

    close s

tests :: [TF.Test]
tests = [
            testGroup "HUnit tests Network.Connection.Conduit" [
                  testCase "sourceConnection" testSourceConnection
                , testCase "sinkConnection" testSinkConnection
            ]
        ]
