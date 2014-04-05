--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Http.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Attoparsec                as A
import qualified Data.ByteString.Char8          as BC
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert,(@?=))


--------------------------------------------------------------------------------
import           Network.WebSockets.Http


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Http.Tests"
    [ testCase "jwebsockets response" jWebSocketsResponse
    , testCase "chromium response"    chromiumResponse
    , testCase "wire protocol parse"  wireProtocolRequest
    ]


--------------------------------------------------------------------------------
-- | This is a specific response sent by jwebsockets which caused trouble
jWebSocketsResponse :: Assertion
jWebSocketsResponse = assert $ case A.parseOnly decodeResponseHead input of
    Left err -> error err
    Right _  -> True
  where
    input = BC.intercalate "\r\n"
        [ "HTTP/1.1 101 Switching Protocols"
        , "Upgrade: websocket"
        , "Connection: Upgrade"
        , "Sec-WebSocket-Accept: Ha0QR1T9CoYx/nqwHsVnW8KVTSo="
        , "Sec-WebSocket-Origin: "
        , "Sec-WebSocket-Location: ws://127.0.0.1"
        , "Set-Cookie: JWSSESSIONID=2e0690e2e328f327056a5676b6a890e3; HttpOnly"
        , ""
        , ""
        ]

--------------------------------------------------------------------------------
-- | This is a specific response sent by chromium which caused trouble
chromiumResponse :: Assertion
chromiumResponse = assert $ case A.parseOnly decodeResponseHead input of
    Left err -> error err
    Right _  -> True
  where
    input = BC.intercalate "\r\n"
        [ "HTTP/1.1 500 Internal Error"
        , "Content-Type:text/html"
        , "Content-Length:23"
        , ""
        , "No such target id: 20_1"
        ]


--------------------------------------------------------------------------------
-- | This is testing the new parsing of the protocol in the request head parser.
--   directly taken from my bug report.    

wireProtocolRequest :: Assertion
wireProtocolRequest = case A.parseOnly (decodeRequestHead False) input of
    Left err -> error err
    Right rh -> requestWireProtocols rh @?= [BinaryWireProtocol,Base64WireProtocol]
  where
    input = BC.intercalate "\r\n"
        [ "GET /websockify HTTP/1.1"
        , "Upgrade: websocket"
        , "Connection: Upgrade"
        , "Host: localhost:5003"
        , "Origin: http://kanaka.github.io"
        , "Sec-WebSocket-Protocol: binary, base64"
        , "Pragma: no-cache"
        , "Cache-Control: no-cache"
        , "Sec-WebSocket-Key: U9FEmmtMOzsE9Zhlzsyyfw=="
        , "Sec-WebSocket-Version: 13"
        , "Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits, x-webkit-deflate-frame"
        , "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36"
        , "Cookie: __ngDebug=false"
        ]
