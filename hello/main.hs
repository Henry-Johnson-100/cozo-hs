{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Monad
import Data.ByteString
import Foreign
import Foreign.C
import MyLib

main :: IO ()
main = do
  useAsCString "mem" $ \engine ->
    useAsCString "" $ \path ->
      useAsCString "{}" $ \options -> do
        intPtr <- new @CInt 0
        mSucc <- cozoOpenDB engine path options intPtr
        maybePeek packCString mSucc
          >>= \case
            Just errStr -> do
              Data.ByteString.putStr errStr
              cozoFreeStr mSucc
            Nothing -> do
              dbId <- peek intPtr
              queryResultPtr <- useAsCString
                "?[] <- [[1, 2, 3]]"
                $ \script ->
                  useAsCString "{}" $ \params ->
                    cozoRunQuery dbId script params (fromBool False)
              maybePeek packCString queryResultPtr
                >>= \case
                  Nothing -> putStrLn "Query returned a nullptr"
                  Just queryResult -> do
                    Data.ByteString.putStr queryResult
              cozoFreeStr queryResultPtr
              b <- cozoCloseDB dbId
              print b
