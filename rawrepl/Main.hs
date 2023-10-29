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
    $ \c -> do 
      putStrLn "Using the raw cozo-hs repl."
      go c
 where
  go conn = do
    x <- getLine
    runQuery conn (fromString x) empty >>= print
    go conn