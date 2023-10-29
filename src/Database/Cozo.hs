{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Cozo (
  -- * Data
  CozoResult (..),
  CozoOkay (..),
  CozoBad (..),
  CozoException (..),

  -- * Function
  open,
  close,
  runQuery,
  backup,
  restore,

  -- * re-export
  CozoNullResultPtrException,
  Database.Cozo.Internal.InternalCozoError,
  Key,
  KeyMap,
  KM.empty,
  KM.singleton,
  KM.insert,
  Value (..),
) where

import Control.Exception (Exception)
import Data.Aeson (
  FromJSON (parseJSON),
  Options (fieldLabelModifier),
  ToJSON (toEncoding),
  Value (..),
  defaultOptions,
  eitherDecodeStrict,
  fromEncoding,
  genericParseJSON,
  withObject,
 )
import Data.Aeson.KeyMap (Key, KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.Char (toLower)
import Data.Text (Text)
import Database.Cozo.Internal
import GHC.Generics (Generic)

data CozoException
  = CozoExceptionInternal InternalCozoError
  | CozoErrorNullPtr CozoNullResultPtrException
  | CozoJSONParseException String
  | CozoOperationFailed Text
  deriving (Show, Eq, Generic)

instance Exception CozoException

newtype CozoErrorReport = CozoErrorReport {runCozoErrorReport :: Maybe Text}
  deriving (Show, Eq, Generic)

instance FromJSON CozoErrorReport where
  parseJSON :: Value -> Parser CozoErrorReport
  parseJSON =
    eitherOkay
      "CozoErrorReport"
      ( withObject
          "CozoErrorReport"
          ( \o ->
              case KM.lookup "message" o of
                Nothing -> fail "Expecting a 'message' field."
                Just m ->
                  case m of
                    String ms -> pure . CozoErrorReport . Just $ ms
                    _ -> fail "Expecting the 'message' field to have a type of String."
          )
      )
      (const (pure (CozoErrorReport Nothing)))

data CozoOkay = CozoOkay
  { cozoOkayHeaders :: [Text]
  , cozoOkayNext :: Maybe Value
  , cozoOkayRows :: [[Value]]
  , cozoOkayTook :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON CozoOkay where
  parseJSON :: Value -> Parser CozoOkay
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 8 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

data CozoBad = CozoBad
  { cozoBadCauses :: [Value]
  , cozoBadCode :: Text
  , cozoBadDisplay :: Text
  , cozoBadMessage :: Text
  , cozoBadRelated :: Value
  , cozoBadSeverity :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON CozoBad where
  parseJSON :: Value -> Parser CozoBad
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 7 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

newtype CozoResult = CozoResult {runCozoResult :: Either CozoBad CozoOkay}
  deriving (Show, Eq, Generic)

instance FromJSON CozoResult where
  parseJSON :: Value -> Parser CozoResult
  parseJSON =
    eitherOkay
      "CozoResult"
      (fmap (CozoResult . Left) . parseJSON)
      (fmap (CozoResult . Right) . parseJSON)

{- |
Open a connection to a cozo database

- engine: "mem", "sqlite" or "rocksdb"
- path: utf8 encoded filepath
- options: engine-specific options. "{}" is an acceptable empty value.
-}
open :: ByteString -> ByteString -> ByteString -> IO (Either CozoException Connection)
open engine path options = first CozoExceptionInternal <$> open' engine path options

{- |
True if the database was closed and False if it was already closed or if it
does not exist.
-}
close :: Connection -> IO Bool
close = close'

{- |
Run a utf8 encoded query with a map of parameters.

Parameters are declared with
text names and can be cany valid JSON type. They are referenced in a query by a '$'
preceding their name.
-}
runQuery ::
  Connection ->
  ByteString ->
  KeyMap Value ->
  IO (Either CozoException CozoResult)
runQuery c query params = do
  r <-
    runQuery'
      c
      query
      ( toStrict
          . toLazyByteString
          . fromEncoding
          . toEncoding
          $ params
      )
  pure $ do
    r' <- first CozoErrorNullPtr r
    cozoDecode r'

{- |
Backup a database.

Accepts the path of the output file.
-}
backup :: Connection -> ByteString -> IO (Either CozoException ())
backup c path =
  decodeCozoCharPtrFn
    <$> backup' c path

{- |
Restore a database from a backup.
-}
restore :: Connection -> ByteString -> IO (Either CozoException ())
restore c path =
  decodeCozoCharPtrFn
    <$> restore' c path

decodeCozoCharPtrFn ::
  Either CozoNullResultPtrException ByteString ->
  Either CozoException ()
decodeCozoCharPtrFn e = do
  e' <- first CozoErrorNullPtr e
  p <- cozoDecode e'
  maybe (pure ()) (Left . CozoOperationFailed) . runCozoErrorReport $ p

cozoDecode :: (FromJSON a) => ByteString -> Either CozoException a
cozoDecode = first CozoJSONParseException . eitherDecodeStrict

{- |
Helper for defining JSON disjunctions that
switch on the value of an 'ok' boolean field.
-}
eitherOkay ::
  String ->
  (Value -> Parser a) ->
  (Value -> Parser a) ->
  Value ->
  Parser a
eitherOkay s l r =
  withObject
    s
    ( \o ->
        case KM.lookup "ok" o of
          Nothing -> fail "Result did not contain \"ok\" field"
          Just ok ->
            case ok of
              Bool b ->
                if b then r (Object o) else l (Object o)
              _ -> fail "\"ok\" field did not contain a Boolean."
    )