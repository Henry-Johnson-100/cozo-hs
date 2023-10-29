{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Redundant case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Cozo (
  runQuery,
  module Database.Cozo.Internal,

  -- * re-export
  KM.empty,
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding),
  Value (Bool),
  decodeStrict,
  fromEncoding,
  withObject,
  (.:),
 )
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Cozo.Internal
import GHC.Generics (Generic)

data CozoOkay = CozoOkay
  { headers :: [Text]
  , next :: Maybe Value
  , rows :: [[Value]]
  , took :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON CozoOkay

data CozoBad = CozoBad
  { causes :: [Value]
  , code :: Text
  , display :: Text
  , message :: Text
  , related :: Value
  , severity :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON CozoBad

instance Exception CozoBad

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

runQuery :: Connection -> Text -> KeyMap Value -> IO (Either CozoBad CozoOkay)
runQuery c q p =
  runQuery'
    c
    (encodeUtf8 q)
    (toStrict . toLazyByteString . fromEncoding . toEncoding $ p)
    False
    >>= either
      throwIO
      ( maybe
          (throwIO CozoNullResultPtrException)
          (pure . runCozoResult)
          . decodeStrict
      )

-- data CozoRuntimeValue
--   = CozoRuntimeNull
--   | CozoRuntimeBool Bool
--   | CozoRuntimeNumber Scientific
--   | CozoRuntimeText Text
--   | CozoRuntimeVec (Vector CozoRuntimeValue)
--   | CozoRuntimeJson (KeyMap CozoRuntimeValue)
--   deriving (Show, Eq, Generic)

-- instance FromJSON CozoRuntimeValue

-- instance ToJSON CozoRuntimeValue

-- data CozoColumnAtomicType
--   = CozoInt
--   | CozoFloat
--   | CozoBool
--   | CozoString
--   | CozoBytes
--   | CozoUuid
--   | CozoJson
--   | CozoValidity
--   deriving (Show, Eq, Generic, Enum, Ord)

-- data CozoColumnCompositeType
--   = CozoHomogeneousList CozoColumnType (Maybe Int)
--   | CozoHeterogeneousList [CozoColumnType]
--   | CozoVec CozoVecType Int
--   deriving (Show, Eq, Generic)

-- data CozoVecType
--   = F32
--   | F64
--   deriving (Show, Eq, Generic)

-- data CozoColumnSumType
--   = CozoAnyColumnType
--   | CozoColumnAtomicType CozoColumnAtomicType
--   | CozoColumnCompositeType CozoColumnCompositeType
--   deriving (Show, Eq, Generic)

-- data CozoColumnType
--   = CozoColumnType CozoColumnSumType Bool
--   deriving (Show, Eq, Generic)