{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Cozo.Internal (
  open',
  close',
  query',
  Connection,
  InternalCozoError (..),
) where

import Control.Exception (Exception)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Coerce (coerce)
import Database.Cozo.Internal.Bindings
import Foreign
import Foreign.C.Types
import GHC.Generics (Generic)

newtype Connection = Connection (Ptr CInt)

{- |
Wrapper around primitive failure states.
-}
data InternalCozoError
  = InternalCozoError ByteString
  | -- | This error corresponds to an unexpected null pointer as a result from a query.
    CozoNullResultPtr
  deriving (Show, Eq, Generic)

instance Exception InternalCozoError

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
  IO (Either InternalCozoError Connection)
open' engineBs pathBs optionBs =
  B.useAsCString engineBs $ \engine ->
    B.useAsCString pathBs $ \path ->
      B.useAsCString optionBs $ \options -> do
        intPtr <- new @CInt 0
        openMessagePtr <- cozoOpenDB engine path options intPtr
        !mOpenMessage <- maybePeek B.packCString openMessagePtr
        maybe
          (pure . Right . Connection $ intPtr)
          (\errStr -> (Left . InternalCozoError $ errStr) <$ cozoFreeStr openMessagePtr)
          mOpenMessage

{- |
True if the database was closed and False if it was already closed or if it
does not exist.
-}
close' :: Connection -> IO Bool
close' = fmap toBool . cozoCloseDB <=< peek . coerce

{- |
Run a query.

The `CozoError` that might arise from this function is a `CozoNullResultPtr`.
If there are any errors internal to cozo, those will be returned as part of the
JSON string returned in the Right value. Simple returning a Right value does not
mean the query was successful.

- script: utf8 encoded script to execute
- params_raw: a utf8 encoded, JSON formatted map of parameters for use in the script.
- b: some boolean that the API is not clear about, set to False.
-}
query' ::
  Connection ->
  ByteString ->
  ByteString ->
  Bool ->
  IO (Either InternalCozoError ByteString)
query' (Connection conPtr) q p b =
  B.useAsCString q $ \q' ->
    B.useAsCString p $ \p' -> do
      dbId <- peek conPtr
      queryResultsPtr <- cozoRunQuery dbId q' p' (fromBool b)
      !mQueryResults <- maybePeek B.packCString queryResultsPtr
      case mQueryResults of
        Nothing -> pure . Left $ CozoNullResultPtr
        Just queryResults -> do
          cozoFreeStr queryResultsPtr
          pure . Right $ queryResults
