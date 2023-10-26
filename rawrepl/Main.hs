{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket, throwIO)
import Control.Monad
import Data.ByteString qualified as B
import Data.String (fromString)
import Database.Cozo

main :: IO ()
main =
  bracket
    ( open' "mem" "" "{}"
        >>= either throwIO pure
    )
    (void . close')
    go
 where
  go conn = do
    x <- getLine
    runQuery' conn (fromString x) "{}" False >>= either print (B.putStr . (<> "\n"))
    go conn