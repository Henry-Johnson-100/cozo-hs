{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Exception (bracket, throwIO)
import Control.Monad (void)
import Data.ByteString (putStr)
import Database.Cozo ( close', open', runQuery' )

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
      runQuery' conn "?[] <- [[1,2,3],['a','b','c']]" "{}" False
        >>= either
          throwIO
          ( Data.ByteString.putStr
              . (<> "\n")
          )
