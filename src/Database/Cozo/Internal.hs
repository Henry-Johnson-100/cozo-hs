{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Cozo.Internal (
  open',
  close',
  query',
  Connection,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Database.Cozo.Internal.Bindings
import Foreign
import Foreign.C.Types

newtype Connection = Connection (Ptr CInt)

{- |
Open a connection to a cozo database

- engine: "mem", "sqlite" or "rocksdb"
- path: utf8 encoded filepath
- options: engine-specific options. "{}" is an acceptable empty value.
-}
open' ::
  -- | engine: 'mem', 'sqlite' or 'rocksdb'
  ByteString ->
  -- | path: utf8 encoded filepath
  ByteString ->
  -- | options: engine-specific options. "{}" is an acceptable emtpy value.
  ByteString ->
  IO (Either ByteString Connection)
open' engineBs pathBs optionBs =
  B.useAsCString engineBs $ \engine ->
    B.useAsCString pathBs $ \path ->
      B.useAsCString optionBs $ \options -> do
        intPtr <- new @CInt 0
        openMessagePtr <- cozoOpenDB engine path options intPtr
        !mOpenMessage <- maybePeek B.packCString openMessagePtr
        maybe
          (pure . Right . Connection $ intPtr)
          (\errStr -> Left errStr <$ cozoFreeStr openMessagePtr)
          mOpenMessage

close' :: Connection -> IO Bool
close' (Connection dbId) = do
  peek dbId >>= fmap toBool . cozoCloseDB

{- |
Run a query

- script: utf8 encoded script to execute
- params_raw: a utf8 encoded, JSON formatted map of parameters for use in the script.
- b: some boolean that the API is not clear about, set to False.
-}
query' :: Connection -> ByteString -> ByteString -> Bool -> IO ByteString
query' (Connection conPtr) q p b =
  B.useAsCString q $ \q' ->
    B.useAsCString p $ \p' -> do
      dbId <- peek conPtr
      queryResultsPtr <- cozoRunQuery dbId q' p' (fromBool b)
      !mQueryResults <- maybePeek B.packCString queryResultsPtr
      case mQueryResults of
        Nothing -> error "The query returned a null pointer"
        Just queryResults -> do
          cozoFreeStr queryResultsPtr
          pure queryResults
