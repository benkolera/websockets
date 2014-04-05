--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO)
import           Control.Exception              (handle)
import           Data.ByteString.Char8          ()
import           Data.IORef                     (newIORef, readIORef,
                                                 writeIORef)
import qualified Data.CaseInsensitive           as CI
import           Data.Maybe                     (fromJust)
import qualified System.IO.Streams.Attoparsec   as Streams
import qualified System.IO.Streams.Builder      as Streams
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@?=), assert)


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Handshake.Test"
    [ testCase "handshake Hybi13"           testHandshakeHybi13 
    , testCase "handshake Hybi13NoProtocol" testHandshakeHybi13NoProtocol
    , testCase "handshake reject"           testHandshakeReject
    , testCase "handshake Hybi9000"         testHandshakeHybi9000
    ]

--------------------------------------------------------------------------------
testHandshake :: RequestHead -> (PendingConnection -> IO a) -> IO ResponseHead
testHandshake rq app = do
    (is, os) <- makeChanPipe
    os'      <- Streams.builderStream os
    _        <- forkIO $ do
        _ <- app (PendingConnection defaultConnectionOptions rq nullify is os')
        return ()
    Streams.parseFromStream decodeResponseHead is
  where
    nullify _ = return ()


--------------------------------------------------------------------------------
(!) :: Eq a => [(a, b)] -> a -> b
assoc ! key = fromJust (lookup key assoc)


--------------------------------------------------------------------------------
rq13 :: RequestHead
rq13 = RequestHead "/mychat"
    [ ("Host", "server.example.com")
    , ("Upgrade", "websocket")
    , ("Connection", "Upgrade")
    , ("Sec-WebSocket-Key", "x3JJHMbDL1EzLkh9GBhXDw==")
    , ("Sec-WebSocket-Version", "13")
    , ("Origin", "http://example.com")
    ]
    False
    [OtherWireProtocol $ CI.mk "chat"]


--------------------------------------------------------------------------------
testHandshakeHybi13 :: Assertion
testHandshakeHybi13 = do
    onAcceptFired                     <- newIORef False
    requestProtocolsSent              <- newIORef []
    ResponseHead code message headers <- testHandshake rq13 $ \pc -> do
        writeIORef requestProtocolsSent . requestWireProtocols . pendingRequest $ pc
        acceptRequest 
          pc {pendingOnAccept = \_ -> writeIORef onAcceptFired True}
          (Just $ OtherWireProtocol "chat")

    readIORef onAcceptFired >>= assert
    readIORef requestProtocolsSent >>= ( @?= [OtherWireProtocol $ CI.mk "chat"])
    code @?= 101
    message @?= "WebSocket Protocol Handshake"
    headers ! "Sec-WebSocket-Accept"        @?= "HSmrc0sMlYUkAGmm5OPpG2HaGWk="
    headers ! "Connection"                  @?= "Upgrade"
    lookup "Sec-WebSocket-Protocol" headers @?= Just "chat"

--------------------------------------------------------------------------------
    
testHandshakeHybi13NoProtocol :: Assertion
testHandshakeHybi13NoProtocol = do
    onAcceptFired                     <- newIORef False
    ResponseHead code message headers <- testHandshake rq13 $ \pc ->
        acceptRequest 
          pc {pendingOnAccept = \_ -> writeIORef onAcceptFired True}
          Nothing

    readIORef onAcceptFired >>= assert
    code @?= 101
    message @?= "WebSocket Protocol Handshake"
    headers ! "Sec-WebSocket-Accept"        @?= "HSmrc0sMlYUkAGmm5OPpG2HaGWk="
    headers ! "Connection"                  @?= "Upgrade"
    lookup "Sec-WebSocket-Protocol" headers @?= Nothing

--------------------------------------------------------------------------------
testHandshakeReject :: Assertion
testHandshakeReject = do
    ResponseHead code _ _ <- testHandshake rq13 $ \pc ->
        rejectRequest pc "YOU SHALL NOT PASS"

    code @?= 400


--------------------------------------------------------------------------------
-- I don't believe this one is supported yet
rq9000 :: RequestHead
rq9000 = RequestHead "/chat"
    [ ("Host", "server.example.com")
    , ("Upgrade", "websocket")
    , ("Connection", "Upgrade")
    , ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
    , ("Sec-WebSocket-Origin", "http://example.com")
    , ("Sec-WebSocket-Version", "9000")
    ]
    False
    [OtherWireProtocol (CI.mk "chat"),OtherWireProtocol (CI.mk "superchat")]


--------------------------------------------------------------------------------
testHandshakeHybi9000 :: Assertion
testHandshakeHybi9000 = do
    ResponseHead code _ headers <- testHandshake rq9000 $ \pc ->
        flip handle (acceptRequest pc Nothing) $ \e -> case e of
            NotSupported -> return undefined
            _            -> error $ "Unexpected Exception: " ++ show e

    code @?= 400
    headers ! "Sec-WebSocket-Version" @?= "13"
