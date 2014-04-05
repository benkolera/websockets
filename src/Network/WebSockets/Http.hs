--------------------------------------------------------------------------------
-- | Module dealing with HTTP: request data types, encoding and decoding...
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Network.WebSockets.Http
    ( Headers
    , RequestHead (..)
    , Request (..)
    , ResponseHead (..)
    , Response (..)
    , HandshakeException (..)
    , WireProtocol (..)

    , encodeRequestHead
    , encodeRequest
    , decodeRequestHead

    , encodeWireProtocolHeader
      
    , encodeResponseHead
    , encodeResponse
    , decodeResponseHead
    , decodeResponse

    , response101
    , response400

    , getRequestHeader
    , getResponseHeader
    , getRequestSecWebSocketVersion
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder           as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import           Control.Applicative                (pure, (*>), (<$>), (<*),
                                                     (<*>))
import           Control.Exception                  (Exception, throw)
import           Control.Monad.Error                (Error (..))
import qualified Data.Attoparsec                    as A
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import           Data.ByteString.Char8              ()
import qualified Data.ByteString.Char8              as BC
import           Data.ByteString.Internal           (c2w)
import qualified Data.CaseInsensitive               as CI
import           Data.Char                          (isSpace)
import           Data.Dynamic                       (Typeable)
import           Data.List                          (find)
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (mappend, mconcat)


--------------------------------------------------------------------------------
-- | Request headers
type Headers = [(CI.CI ByteString, ByteString)]
data WireProtocol =
  BinaryWireProtocol
  | Base64WireProtocol
  | OtherWireProtocol (CI.CI ByteString)
  deriving (Show,Eq)

--------------------------------------------------------------------------------
-- | An HTTP request. The request body is not yet read.
data RequestHead = RequestHead
    { requestPath          :: !B.ByteString
    , requestHeaders       :: Headers
    , requestSecure        :: Bool
    , requestWireProtocols :: [WireProtocol]
    } deriving (Show)


--------------------------------------------------------------------------------
-- | A request with a body
data Request = Request RequestHead B.ByteString
    deriving (Show)


--------------------------------------------------------------------------------
-- | HTTP response, without body.
data ResponseHead = ResponseHead
    { responseCode    :: !Int
    , responseMessage :: !B.ByteString
    , responseHeaders :: Headers
    } deriving (Show)


--------------------------------------------------------------------------------
-- | A response including a body
data Response = Response ResponseHead B.ByteString
    deriving (Show)


--------------------------------------------------------------------------------
-- | Error in case of failed handshake. Will be thrown as an 'Exception'.
--
-- TODO: This should probably be in the Handshake module, and is solely here to
-- prevent a cyclic dependency.
data HandshakeException
    -- | We don't have a match for the protocol requested by the client.
    -- todo: version parameter
    = NotSupported
    -- | The request was somehow invalid (missing headers or wrong security
    -- token)
    | MalformedRequest RequestHead String
    -- | The servers response was somehow invalid (missing headers or wrong
    -- security token)
    | MalformedResponse ResponseHead String
    -- | The request was well-formed, but the library user rejected it.
    -- (e.g. "unknown path")
    | RequestRejected Request String
    -- | for example "EOF came too early" (which is actually a parse error)
    -- or for your own errors. (like "unknown path"?)
    | OtherHandshakeException String
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Error HandshakeException where
    strMsg = OtherHandshakeException


--------------------------------------------------------------------------------
instance Exception HandshakeException


--------------------------------------------------------------------------------
encodeRequestHead :: RequestHead -> Builder.Builder
encodeRequestHead (RequestHead path headers _ protocols) =
    Builder.copyByteString "GET "          `mappend`
    Builder.copyByteString path            `mappend`
    Builder.copyByteString " HTTP/1.1"     `mappend`
    Builder.fromByteString "\r\n"          `mappend`
    mconcat (map header headersWProtocols) `mappend`
    Builder.copyByteString "\r\n"
  where
    headersWProtocols = case protocols of
      [] -> headers
      ps -> encodeWireProtocolHeader ps :
            filter ((== wireProtocolHeaderName) . fst) headers
   
    header (k, v) = mconcat $ map Builder.copyByteString
        [CI.original k, ": ", v, "\r\n"]

encodeWireProtocolHeader ps =
  ( wireProtocolHeaderName , B.intercalate "," $ fmap encodeWireProtocol ps )

encodeWireProtocol BinaryWireProtocol     = "binary"
encodeWireProtocol Base64WireProtocol     = "base64"
encodeWireProtocol (OtherWireProtocol ci) = CI.original ci

--------------------------------------------------------------------------------
encodeRequest :: Request -> Builder.Builder
encodeRequest (Request head' body) =
    encodeRequestHead head' `mappend` Builder.copyByteString body


--------------------------------------------------------------------------------
-- | Parse an initial request
decodeRequestHead :: Bool -> A.Parser RequestHead
decodeRequestHead isSecure = mkRequestHead
    <$> requestLine
    <*> A.manyTill decodeHeaderLine newline
    <*> pure isSecure
  where
    mkRequestHead path headers secure = 
      RequestHead 
        path 
        (filter ((/= wireProtocolHeaderName) . fst) headers)
        secure
        (wireProtocolHeaders headers >>= parseWireProtocols)

    space   = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    parseWireProtocols = fmap (parseWireProtocol . CI.mk) . commaSplit
    commaSplit = fmap (BC.dropWhile isSpace) . BC.split ','
    parseWireProtocol ci =
      fromMaybe (OtherWireProtocol ci) $ lookup ci [
        (CI.mk "binary",BinaryWireProtocol)
        ,(CI.mk "base64",Base64WireProtocol)
        ]

    wireProtocolHeaders = fmap snd . filter ((== wireProtocolHeaderName) . fst)

    requestLine = A.string "GET" *> space *> A.takeWhile1 (/= c2w ' ')
        <* space
        <* A.string "HTTP/1.1" <* newline


--------------------------------------------------------------------------------
-- | Encode an HTTP upgrade response
encodeResponseHead :: ResponseHead -> Builder.Builder
encodeResponseHead (ResponseHead code msg headers) =
    Builder.copyByteString "HTTP/1.1 " `mappend`
    Builder.fromString (show code)     `mappend`
    Builder.fromChar ' '               `mappend`
    Builder.fromByteString msg         `mappend`
    Builder.fromByteString "\r\n"      `mappend`
    mconcat (map header headers)       `mappend`
    Builder.copyByteString "\r\n"
  where
    header (k, v) = mconcat $ map Builder.copyByteString
        [CI.original k, ": ", v, "\r\n"]


--------------------------------------------------------------------------------
encodeResponse :: Response -> Builder.Builder
encodeResponse (Response head' body) =
    encodeResponseHead head' `mappend` Builder.copyByteString body


--------------------------------------------------------------------------------
-- | An upgrade response
response101 :: Headers -> B.ByteString -> Response
response101 headers = Response
    (ResponseHead 101 "WebSocket Protocol Handshake"
        (("Upgrade", "websocket") : ("Connection", "Upgrade") : headers))


--------------------------------------------------------------------------------
-- | Bad request
response400 :: Headers -> B.ByteString -> Response
response400 headers = Response (ResponseHead 400 "Bad Request" headers)


--------------------------------------------------------------------------------
-- | HTTP response parser
decodeResponseHead :: A.Parser ResponseHead
decodeResponseHead = ResponseHead
    <$> fmap (read . BC.unpack) code
    <*> message
    <*> A.manyTill decodeHeaderLine newline
  where
    space = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    code = A.string "HTTP/1.1" *> space *> A.takeWhile1 (/= c2w ' ') <* space
    message = A.takeWhile1 (/= c2w '\r') <* newline


--------------------------------------------------------------------------------
decodeResponse :: A.Parser Response
decodeResponse = Response <$> decodeResponseHead <*> A.takeByteString


--------------------------------------------------------------------------------
getRequestHeader :: RequestHead
                 -> CI.CI ByteString
                 -> ByteString
getRequestHeader rq key = case lookup key (requestHeaders rq) of
    Just t  -> t
    Nothing -> throw $ MalformedRequest rq $
        "Header missing: " ++ BC.unpack (CI.original key)


--------------------------------------------------------------------------------
getResponseHeader :: ResponseHead
                  -> CI.CI ByteString
                  -> ByteString
getResponseHeader rsp key = case lookup key (responseHeaders rsp) of
    Just t  -> t
    Nothing -> throw $ MalformedResponse rsp $
        "Header missing: " ++ BC.unpack (CI.original key)


--------------------------------------------------------------------------------
-- | Get the @Sec-WebSocket-Version@ header
getRequestSecWebSocketVersion :: RequestHead -> Maybe B.ByteString
getRequestSecWebSocketVersion p =
    lookup "Sec-WebSocket-Version" (requestHeaders p)


--------------------------------------------------------------------------------
decodeHeaderLine :: A.Parser (CI.CI ByteString, ByteString)
decodeHeaderLine = (,)
    <$> (CI.mk <$> A.takeWhile1 (/= c2w ':'))
    <*  A.word8 (c2w ':')
    <*  A.option (c2w ' ') (A.word8 (c2w ' '))
    <*> A.takeWhile (/= c2w '\r')
    <*  A.string "\r\n"

--------------------------------------------------------------------------------

wireProtocolHeaderName :: CI.CI ByteString
wireProtocolHeaderName = CI.mk "Sec-WebSocket-Protocol"
