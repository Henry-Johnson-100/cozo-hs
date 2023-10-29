{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Database.Cozo (
  -- * Data
  CozoResult (..),
  CozoOkay (..),
  CozoBad (..),
  CozoResultParseError (..),

  -- * Function
  open,
  close,
  runQuery,

  -- * re-export
  Key,
  KeyMap,
  KM.empty,
  KM.singleton,
  KM.insert,
  Value (..),
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding),
  Value (Bool),
  eitherDecodeStrict,
  fromEncoding,
  withObject,
  (.:),
 )
import Data.Aeson.KeyMap (Key, KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Cozo.Internal
import GHC.Generics (Generic)

newtype CozoResultParseError = CozoResultParseError String
  deriving (Show, Eq, Generic)

instance Exception CozoResultParseError

data CozoOkay = CozoOkay
  { headers :: [Text]
  , next :: Maybe Value
  , rows :: [[Value]]
  , took :: Double
  }
  deriving (Show, Eq, Generic)

data CozoBad = CozoBad
  { causes :: [Value]
  , code :: Text
  , display :: Text
  , message :: Text
  , related :: Value
  , severity :: Text
  }
  deriving (Show, Eq, Generic)

newtype CozoResult = CozoResult {runCozoResult :: Either CozoBad CozoOkay}
  deriving (Show, Eq, Generic)

instance FromJSON CozoResult where
  parseJSON :: Value -> Parser CozoResult
  parseJSON = withObject "CozoResult" $ \v ->
    case KM.lookup "ok" v of
      Nothing -> fail "Result did not contain \"ok\" field"
      Just ok ->
        case ok of
          Bool b ->
            coerce
              $ if b
                then
                  Right
                    <$> ( CozoOkay
                            <$> v
                            .: "headers"
                            <*> v
                            .: "next"
                            <*> v
                            .: "rows"
                            <*> v
                            .: "took"
                        )
                else
                  Left
                    <$> ( CozoBad
                            <$> v
                            .: "causes"
                            <*> v
                            .: "code"
                            <*> v
                            .: "display"
                            <*> v
                            .: "message"
                            <*> v
                            .: "related"
                            <*> v
                            .: "severity"
                        )
          _ -> fail "\"ok\" field did not contain a Boolean."

{- |
Open a connection to a cozo database

- engine: "mem", "sqlite" or "rocksdb"
- path: utf8 encoded filepath
- options: engine-specific options. "{}" is an acceptable empty value.
-}
open :: ByteString -> ByteString -> ByteString -> IO Connection
open engine path options = open' engine path options >>= either throwIO pure

{- |
True if the database was closed and False if it was already closed or if it
does not exist.
-}
close :: Connection -> IO Bool
close = close'

{- |
Run a utf8 encoded query with a map of parameters.

Throws an error if the result could not be parsed. Otherwise returns
  a newtype wrapper over an @Either CozoBad CozoOkay@ which denotes the
  success state of the given query.

The only reason that a parse error may be thrown from this function is some
unexpected field or absence of an expected field in the return type which would be
an abnormality of the underlying API.

Parameters are declared with
text names and can be cany valid JSON type. They are referenced in a query by a '$'
preceding their name.

May throw a `CozoNullResultPtrException` if the query returns a null pointer.
I don't believe that this will ever happen.
-}
runQuery ::
  Connection ->
  ByteString ->
  KeyMap Value ->
  IO CozoResult
runQuery c query params = do
  r <-
    either throwIO pure
      =<< runQuery'
        c
        query
        (toStrict . toLazyByteString . fromEncoding . toEncoding $ params)
  either (throwIO . CozoResultParseError) pure . eitherDecodeStrict $ r
