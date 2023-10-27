{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Cozo.Internal (
  open',
  close',
  runQuery',
  importRelations',
  exportRelations',
  backup',
  restore',
  importFromBackup',
  Connection,
  InternalCozoError (..),
) where

import Control.Exception (Exception)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Coerce (coerce)
import Database.Cozo.Internal.Bindings (
  cozoBackup,
  cozoCloseDB,
  cozoExportRelations,
  cozoFreeStr,
  cozoImportFromBackup,
  cozoImportRelations,
  cozoOpenDB,
  cozoRestore,
  cozoRunQuery,
 )
import Foreign (
  Ptr,
  Storable (peek),
  fromBool,
  maybePeek,
  new,
  toBool,
 )
import Foreign.C.Types (CChar, CInt)
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
runQuery' ::
  Connection ->
  ByteString ->
  ByteString ->
  Bool ->
  IO (Either InternalCozoError ByteString)
runQuery' c q p b =
  B.useAsCString q $ \q' ->
    B.useAsCString p $ \p' ->
      cozoCharPtrFn (\i -> cozoRunQuery i q' p' (fromBool b)) c

{- |
Import data in relations.

Triggers are not run for relations, if you wish to activate triggers, use a query
  with parameters.

The given bytestring is a utf8, JSON formatted payload of relations.
In the same form as that given by `exportRelations'`
-}
importRelations' :: Connection -> ByteString -> IO (Either InternalCozoError ByteString)
importRelations' c payloadBs =
  B.useAsCString payloadBs $ \payload ->
    cozoCharPtrFn (`cozoImportRelations` payload) c

{- |
Export relations into JSON

The given bytestring must be a utf8 encoded JSON payload. See the manual for expected
fields.
-}
exportRelations' :: Connection -> ByteString -> IO (Either InternalCozoError ByteString)
exportRelations' c payloadBs =
  B.useAsCString payloadBs $ \payload ->
    cozoCharPtrFn (`cozoExportRelations` payload) c

{- |
Backup a database.

Accepts the path of the output file.
-}
backup' :: Connection -> ByteString -> IO (Either InternalCozoError ByteString)
backup' c pathBs =
  B.useAsCString pathBs $ \path ->
    cozoCharPtrFn (`cozoBackup` path) c

{- |
Restore a database from a backup.
-}
restore' :: Connection -> ByteString -> IO (Either InternalCozoError ByteString)
restore' c pathBs =
  B.useAsCString pathBs $ \path ->
    cozoCharPtrFn (`cozoRestore` path) c

{- |
Import relations from a backup.

Note that triggers are not run for the relations.
To run triggers, use a query with parameters.

- payload: @"{'path': ..., 'relations': [...]}"@
-}
importFromBackup' :: Connection -> ByteString -> IO (Either InternalCozoError ByteString)
importFromBackup' c payloadBs =
  B.useAsCString payloadBs $ \payload ->
    cozoCharPtrFn (`cozoImportFromBackup` payload) c

{- |
Helper function for using cozo bindings that do an action and return a
string that needs to be freed.
-}
cozoCharPtrFn ::
  (CInt -> IO (Ptr CChar)) ->
  Connection ->
  IO (Either InternalCozoError ByteString)
cozoCharPtrFn a (Connection intPtr) = do
  dbId <- peek intPtr
  rPtr <- a dbId
  !mR <- maybePeek B.packCString rPtr
  case mR of
    Nothing -> pure . Left $ CozoNullResultPtr
    Just r -> Right r <$ cozoFreeStr rPtr