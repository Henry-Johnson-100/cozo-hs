{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Database.Cozo
Description : Wrappers and types for the Cozo C API
License     : MPL-2.0
Maintainer  : hencutJohnson@gmail.com

Included are some wrapping functions for Cozo's C API and data types to deserialize them.
-}
module Database.Cozo (
  -- * Data
  CozoResult (..),
  CozoOkay (..),
  CozoBad (..),
  CozoRelationExportPayload (..),
  CozoRelationExport (..),
  CozoException (..),

  -- * Functions
  open,
  close,
  runQuery,
  backup,
  restore,
  importRelations,
  exportRelations,
  importFromBackup,

  -- ** Lower Level Wrappers
  open',
  close',
  runQuery',
  backup',
  restore',
  importRelations',
  exportRelations',
  importFromBackup',

  -- * Re-exports
  Connection,
  CozoNullResultPtrException,
  Database.Cozo.Internal.InternalCozoError,
  Key,
  KeyMap,
  KM.empty,
  KM.singleton,
  KM.insert,
  KM.fromList,
  Value (..),
) where

import Control.Exception (Exception)
import Data.Aeson (
  FromJSON (parseJSON),
  Options (fieldLabelModifier),
  ToJSON (..),
  Value (..),
  defaultOptions,
  eitherDecodeStrict,
  fromEncoding,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
  withObject,
 )
import Data.Aeson.KeyMap (Key, KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Encoding, Parser)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Cozo.Internal
import GHC.Generics (Generic)

data ConstJSON = ConstJSON deriving (Show, Eq, Generic)

instance FromJSON ConstJSON where
  parseJSON :: Value -> Parser ConstJSON
  parseJSON _ = pure ConstJSON

{- |
A failure that cannot be recovered from easily.
-}
data CozoException
  = -- | An internal error may occur when a connection is first being established
    -- but not after that.
    CozoExceptionInternal InternalCozoError
  | -- | If any operation in the underlying C API returns a null pointer instead
    -- of a pointer to a valid string, this error will be returned.
    CozoErrorNullPtr CozoNullResultPtrException
  | -- | The result of any operation fails to be deserialized appropriately.
    -- This is a problem with the wrapper for the API and should be
    -- submitted as an issue if it ever arises.
    CozoJSONParseException String
  | -- | A non-query operation such as a backup or import failed.
    -- These usually occur because the user is trying to import or export a
    -- relation that does not exist in the target database.
    CozoOperationFailed Text
  deriving (Show, Eq, Generic)

instance Exception CozoException

newtype CozoMessage = CozoMessage {runCozoMessage :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON CozoMessage where
  parseJSON :: Value -> Parser CozoMessage
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 7 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

cozoMessageToException :: CozoMessage -> CozoException
cozoMessageToException (CozoMessage m) = CozoOperationFailed m

{- |
The rows of some relation with its headers.
-}
data CozoRelationExport = CozoRelationExport
  { cozoRelationExportHeaders :: [Text]
  , cozoRelationExportNext :: Value
  , cozoRelationExportRows :: [[Value]]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CozoRelationExport where
  parseJSON :: Value -> Parser CozoRelationExport
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 18 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

instance ToJSON CozoRelationExport where
  toJSON :: CozoRelationExport -> Value
  toJSON =
    genericToJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 18 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

  toEncoding :: CozoRelationExport -> Encoding
  toEncoding =
    genericToEncoding
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 18 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

{- |
A map of names and the relations they contain.
This type is intended to be used as input to an import function
or otherwise stored as JSON.
-}
newtype CozoRelationExportPayload = CozoRelationExportPayload
  { cozoRelationExportPayloadData :: KeyMap CozoRelationExport
  }
  deriving (Show, Eq, Generic)

instance FromJSON CozoRelationExportPayload where
  parseJSON :: Value -> Parser CozoRelationExportPayload
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 25 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

{- |
An intermediate type for decoding structures that return an object with a 'message' field
when the 'ok' field is false.
-}
newtype IntermediateCozoMessageOnNotOK a = IntermediateCozoMessageOnNotOK
  { runIntermediateCozoMessageOnNotOK :: Either CozoMessage a
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (IntermediateCozoMessageOnNotOK a) where
  parseJSON :: Value -> Parser (IntermediateCozoMessageOnNotOK a)
  parseJSON =
    eitherOkay
      "IntermediateCozoRelationExport"
      (fmap (IntermediateCozoMessageOnNotOK . Left) . parseJSON)
      (fmap (IntermediateCozoMessageOnNotOK . Right) . parseJSON)

data IntermediateCozoImportFromRelationInput = IntermediateCozoImportFromRelationInput
  { intermediateCozoImportFromRelationInputPath :: Text
  , intermediateCozoImportFromRelationInputRelations :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON IntermediateCozoImportFromRelationInput where
  toJSON :: IntermediateCozoImportFromRelationInput -> Value
  toJSON =
    genericToJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 39 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )
  toEncoding :: IntermediateCozoImportFromRelationInput -> Encoding
  toEncoding =
    genericToEncoding
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 39 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

{- |
An intermediate type for packing a list of named relations into an object with the form
\"{'relations': [...]}\"
-}
newtype IntermediateCozoRelationInput = IntermediateCozoRelationInput
  { intermediateCozoRelationInputRelations :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON IntermediateCozoRelationInput where
  toJSON :: IntermediateCozoRelationInput -> Value
  toJSON =
    genericToJSON
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 29 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )
  toEncoding ::
    IntermediateCozoRelationInput ->
    Encoding
  toEncoding =
    genericToEncoding
      ( defaultOptions
          { fieldLabelModifier = \s ->
              case drop 29 s of
                [] -> []
                x : xs -> toLower x : xs
          }
      )

{- |
An okay result from a query.

Contains result headers, and rows among other things.
-}
data CozoOkay = CozoOkay
  { cozoOkayHeaders :: [Text]
  , cozoOkayNext :: Value
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

{- |
A bad result from a query.

Contains information on what went wrong.
-}
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

- engine: \"mem\", \"sqlite\" or \"rocksdb\"
- path: utf8 encoded filepath
- options: engine-specific options. \"{}\" is an acceptable empty value.
-}
open :: Text -> Text -> Text -> IO (Either CozoException Connection)
open engine path options =
  first CozoExceptionInternal
    <$> open'
      (encodeUtf8 engine)
      (encodeUtf8 path)
      (encodeUtf8 options)

{- |
True if the database was closed and False if it was already closed or if it
does not exist.
-}
close :: Connection -> IO Bool
close = close'

{- |
Run a utf8 encoded query with a map of parameters.

Parameters are declared with
text names and can be any valid JSON type. They are referenced in a query by a \"$\"
preceding their name.
-}
runQuery ::
  Connection ->
  Text ->
  KeyMap Value ->
  IO (Either CozoException CozoResult)
runQuery c query params = do
  r <-
    runQuery'
      c
      (encodeUtf8 query)
      ( toStrict
          . toLazyByteString
          . fromEncoding
          . toEncoding
          $ params
      )
  pure $ first CozoErrorNullPtr r >>= cozoDecode

{- |
Backup a database.

Accepts the path of the output file.
-}
backup :: Connection -> Text -> IO (Either CozoException ())
backup c path =
  decodeCozoCharPtrFn
    <$> backup' c (encodeUtf8 path)

{- |
Restore a database from a backup.
-}
restore :: Connection -> Text -> IO (Either CozoException ())
restore c path =
  decodeCozoCharPtrFn
    <$> restore' c (encodeUtf8 path)

{- |
Import data in relations.

Triggers are not run for relations, if you wish to activate triggers, use a query
  with parameters.
-}
importRelations ::
  Connection ->
  CozoRelationExportPayload ->
  IO (Either CozoException ())
importRelations c (CozoRelationExportPayload km) =
  decodeCozoCharPtrFn
    <$> importRelations' c (strictToEncoding km)

{- |
Export the relations specified by the given names.
-}
exportRelations ::
  Connection ->
  [Text] ->
  IO (Either CozoException CozoRelationExportPayload)
exportRelations c bs = do
  r <-
    exportRelations'
      c
      ( strictToEncoding
          . IntermediateCozoRelationInput
          $ bs
      )
  pure
    $ first CozoErrorNullPtr r
    >>= first CozoJSONParseException
    . eitherDecodeStrict @(IntermediateCozoMessageOnNotOK CozoRelationExportPayload)
    >>= first cozoMessageToException
    . runIntermediateCozoMessageOnNotOK

{- |
Import the relations corresponding to the given names
from the specified path.
-}
importFromBackup :: Connection -> Text -> [Text] -> IO (Either CozoException ())
importFromBackup c path relations =
  decodeCozoCharPtrFn
    <$> importFromBackup'
      c
      (strictToEncoding $ IntermediateCozoImportFromRelationInput path relations)

decodeCozoCharPtrFn ::
  Either CozoNullResultPtrException ByteString ->
  Either CozoException ()
decodeCozoCharPtrFn e =
  first CozoErrorNullPtr e
    >>= cozoDecode @(IntermediateCozoMessageOnNotOK ConstJSON)
    >>= bimap cozoMessageToException (const ())
    . runIntermediateCozoMessageOnNotOK

cozoDecode :: (FromJSON a) => ByteString -> Either CozoException a
cozoDecode = first CozoJSONParseException . eitherDecodeStrict

{- |
Helper for defining JSON disjunctions that
switch on the value of an \"ok\" boolean field.
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

strictToEncoding :: (ToJSON a) => a -> ByteString
strictToEncoding =
  toStrict
    . toLazyByteString
    . fromEncoding
    . toEncoding