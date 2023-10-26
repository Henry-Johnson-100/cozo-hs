{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Control.Monad
import Data.ByteString qualified as B
import Data.String (fromString)
import Database.Cozo

main :: IO ()
main =
  bracket
    ( open' "mem" "" "{}"
        >>= either (error "There was a problem opening a connection" . B.putStr) pure
    )
    (void . close')
    go
 where
  go conn = do
    x <- getLine
    query' conn (fromString x) "{}" False >>= B.putStr . (<> "\n")
    go conn