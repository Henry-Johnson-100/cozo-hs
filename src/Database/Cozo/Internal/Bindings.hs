{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}

module Database.Cozo.Internal.Bindings (
  cozoOpenDB,
  cozoCloseDB,
  cozoRunQuery,
  cozoImportRelations,
  cozoExportRelations,
  cozoBackup,
  cozoRestore,
  cozoImportFromBackup,
  cozoFreeStr,
) where

import Foreign.C.Types (CBool (..), CChar, CInt (..))
import Foreign.Ptr (Ptr)

foreign import capi "cozo_c.h cozo_open_db"
  cozoOpenDB :: Ptr CChar -> Ptr CChar -> Ptr CChar -> Ptr CInt -> IO (Ptr CChar)

foreign import capi "cozo_c.h cozo_close_db"
  cozoCloseDB :: CInt -> IO CBool

foreign import capi "cozo_c.h cozo_run_query"
  cozoRunQuery :: CInt -> Ptr CChar -> Ptr CChar -> CBool -> IO (Ptr CChar)

foreign import capi "cozo_c.h cozo_import_relations"
  cozoImportRelations :: CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import capi "cozo_c.h cozo_export_relations"
  cozoExportRelations :: CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import capi "cozo_c.h cozo_backup"
  cozoBackup :: CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import capi "cozo_c.h cozo_restore"
  cozoRestore :: CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import capi "cozo_c.h cozo_import_from_backup"
  cozoImportFromBackup :: CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import capi "cozo_c.h cozo_free_str"
  cozoFreeStr :: Ptr CChar -> IO ()