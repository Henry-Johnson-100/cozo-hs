{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Exception (bracket, throwIO)
import Control.Monad (void)
import Data.ByteString (putStr)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Database.Cozo

main :: IO ()
main =
  bracket
    ( open "mem" "" "{}" >>= either throwIO pure
    )
    (void . close)
    $ \conn -> do
      either
        (print . cozoBadMessage)
        (traverse_ (putStrLn . intercalate ", " . map show) . cozoOkayRows)
        . coerce
        =<< either throwIO pure
        =<< runQuery conn "?[] <- [[1,2,3], ['a','b','c']]" empty
