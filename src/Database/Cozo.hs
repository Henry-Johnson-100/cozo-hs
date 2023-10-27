{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Redundant case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Cozo (
  module Database.Cozo.Internal,
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (Parser)
import Data.ByteString
import Data.Int (Int64)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Database.Cozo.Internal
import GHC.Generics (Generic)

data CozoRuntimeValue
  = CozoRuntimeNull
  | CozoRuntimeBool Bool
  | CozoRuntimeNumber Scientific
  | CozoRuntimeText Text
  | CozoRuntimeVec (Vector CozoRuntimeValue)
  | CozoRuntimeJson (KeyMap CozoRuntimeValue)
  deriving (Show, Eq, Generic)

instance FromJSON CozoRuntimeValue

instance ToJSON CozoRuntimeValue

data CozoColumnAtomicType
  = CozoInt
  | CozoFloat
  | CozoBool
  | CozoString
  | CozoBytes
  | CozoUuid
  | CozoJson
  | CozoValidity
  deriving (Show, Eq, Generic, Enum, Ord)

data CozoColumnCompositeType
  = CozoHomogeneousList CozoColumnType (Maybe Int)
  | CozoHeterogeneousList [CozoColumnType]
  | CozoVec CozoVecType Int
  deriving (Show, Eq, Generic)

data CozoVecType
  = F32
  | F64
  deriving (Show, Eq, Generic)

data CozoColumnSumType
  = CozoAnyColumnType
  | CozoColumnAtomicType CozoColumnAtomicType
  | CozoColumnCompositeType CozoColumnCompositeType
  deriving (Show, Eq, Generic)

data CozoColumnType
  = CozoColumnType CozoColumnSumType Bool
  deriving (Show, Eq, Generic)