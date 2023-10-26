{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Exception (bracket)
import Control.Monad (void, (<=<))
import Data.ByteString (putStr)
import Database.Cozo

main :: IO ()
main =
  bracket
    ( open' "mem" "" "{}"
        >>= either
          (error "Problem opening the database." <=< Data.ByteString.putStr)
          pure
    )
    (void . close')
    $ \conn -> do
      query' conn "?[] <- [[1,2,3],['a','b','c']]" "{}" False
        >>= Data.ByteString.putStr
        . (<> "\n")
