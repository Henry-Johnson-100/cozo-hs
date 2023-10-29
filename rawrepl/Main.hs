{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant case" #-}

import Control.Exception (bracket, throwIO)
import Control.Monad
import Data.ByteString qualified as B
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.String (fromString)
import Database.Cozo

main :: IO ()
main =
  bracket
    (open "mem" "" "{}")
    (void . close)
    $ \c -> do
      putStrLn "Using the raw cozo-hs repl."
      go c
 where
  go conn = do
    getLine >>= putQueryResults
    go conn
   where
    putQueryResults q =
      either
        (print . message)
        (traverse_ (putStrLn . intercalate ", " . map show) . rows)
        . coerce
        =<< runQuery conn (fromString q) empty