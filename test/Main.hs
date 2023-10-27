{-# LANGUAGE OverloadedStrings #-}

import Database.Cozo
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain
    . testGroup "cozo-hs Tests"
    $ [ connectionTests
      ]

connectionTests :: TestTree
connectionTests =
  testGroup
    "Connection Tests"
    [ testCase "Establish Connection" $ do
        ec <- open' "mem" "" "{}"
        case ec of
          Left err -> assertFailure . show $ err
          Right c ->
            close' c
              >>= assertBool "Database was already closed or did not exist."
    ]