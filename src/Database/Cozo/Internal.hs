{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Cozo.Internal (
  open',
  close',
  query',
  Engine (..),
  Connection,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Database.Cozo.Internal.Bindings
import Foreign
import Foreign.C.Types

data Engine
  = Mem
  | SQLite
  | RocksDB
  deriving (Show, Eq, Ord, Bounded, Enum)

newtype Connection = Connection (Ptr CInt)

open' :: Engine -> ByteString -> IO (Either ByteString Connection)
open' engine p =
  B.useAsCString engineToByteString $ \engineStr ->
    B.useAsCString p $ \path ->
      B.useAsCString "{}" $ \options -> do
        intPtr <- new @CInt 0
        openMessagePtr <- cozoOpenDB engineStr path options intPtr
        !mOpenMessage <- maybePeek B.packCString openMessagePtr
        case mOpenMessage of
          Just openMessage -> do
            cozoFreeStr openMessagePtr
            pure . Left $ openMessage
          Nothing -> do
            pure . Right . Connection $ intPtr
 where
  engineToByteString =
    case engine of
      Mem -> "mem"
      SQLite -> "sqlite"
      RocksDB -> "rocksdb"

close' :: Connection -> IO Bool
close' (Connection dbId) = do
  peek dbId >>= fmap toBool . cozoCloseDB

query' :: Connection -> ByteString -> ByteString -> IO ByteString
query' (Connection conPtr) q p =
  B.useAsCString q $ \q' ->
    B.useAsCString p $ \p' -> do
      dbId <- peek conPtr
      queryResultsPtr <- cozoRunQuery dbId q' p' (fromBool False)
      !mQueryResults <- maybePeek B.packCString queryResultsPtr
      case mQueryResults of
        Nothing -> error "The query returned a null pointer"
        Just queryResults -> do
          cozoFreeStr queryResultsPtr
          pure queryResults
