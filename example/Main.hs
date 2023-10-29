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
    ( open' "mem" "" "{}"
        >>= either
          throwIO
          pure
    )
    (void . close')
    $ \conn -> do
      either
        throwIO
        ( either
            (print . message)
            (traverse_ (putStrLn . intercalate ", " . map show) . rows)
            . coerce
        )
        =<< runQuery conn "?[] <- [[1,2,3], ['a','b','c']]" empty
