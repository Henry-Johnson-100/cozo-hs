{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

import Control.Exception (bracket, throwIO)
import Database.Cozo
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain
    . testGroup "cozo-hs Tests"
    $ [ connectionTests
      , exportAndImportTests
      ]

connectionTests :: TestTree
connectionTests =
  testGroup
    "Connection Tests"
    [ testCase "Establish Connection"
        $ open "mem" "" "{}"
        >>= either throwIO pure
        >>= close
        >>= assertBool "Database was already closed or did not exist."
    ]

exportAndImportTests :: TestTree
exportAndImportTests =
  testGroup
    "Export and Import Tests"
    [ testCase "Can export and import" $ do
        e <-
          testWithConnection
            $ \c -> do
              _ <-
                runQuery
                  c
                  "?[foo] <- [['foo']]; \
                  \:create Foo{foo:String}"
                  empty
                  >>= either throwIO pure
              exportRelations c ["Foo"] >>= either throwIO pure
        (CozoResult (Right o)) <- testWithConnection $ \c -> do
          _ <- runQuery c ":create Foo{foo:String}" empty >>= either throwIO pure
          _ <- importRelations c e
          runQuery c "?[foo] := *Foo{foo}" empty >>= either throwIO pure

        assertEqual
          "Expecting a single relation to be exported."
          [[String "foo"]]
          . cozoOkayRows
          $ o
    ]

testWithConnection =
  bracket
    (open "mem" "" "{}" >>= either throwIO pure)
    ((() <$) . close)