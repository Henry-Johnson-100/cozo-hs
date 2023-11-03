{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

import Control.Exception (bracket, throwIO)
import Data.Aeson (toJSON)
import Data.Either (isLeft)
import Database.Cozo
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain
    . testGroup "cozo-hs Tests"
    $ [ connectionTests
      , withResource
          (open "mem" "" "{}" >>= either throwIO pure)
          ((() <$) . close)
          runQueryTests
      , exportAndImportTests
      ]

connectionTests :: (HasCallStack) => TestTree
connectionTests =
  testGroup
    "Connection Tests"
    [ testCase "Establish Connection"
        $ open "mem" "" "{}"
        >>= either throwIO pure
        >>= close
        >>= assertBool "Database was already closed or did not exist."
    ]

exportAndImportTests :: (HasCallStack) => TestTree
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
          . namedRowsRows
          . cozoOkayNamedRows
          $ o
    ]

runQueryTests :: (HasCallStack) => IO Connection -> TestTree
runQueryTests ioc =
  testGroup
    "runQuery Tests"
    [ testCase "Example Query" $ do
        c <- ioc
        rs <-
          unsafeRows
            =<< runQuery c "?[] <- [[1,2,3], ['a', 'b', 'c']]" empty
        assertEqual
          "Expecting a literal query to return valid results."
          [toJSON @Int <$> [1, 2, 3], toJSON @String <$> ["a", "b", "c"]]
          rs
    , testCase "Parameterized Query" $ do
        c <- ioc
        rs <-
          unsafeRows
            =<< runQuery c "?[] <- [[$foo]]" (singleton "foo" (toJSON @String "foo"))
        assertEqual "" [[String "foo"]] rs
    , testCase "Can Capture Error Messages" $ do
        c <- ioc
        (Right (CozoResult cr)) <- runQuery c "?[] <- [[1,2,3]" empty
        assertBool "Expecting a failure for incorrect query syntax" (isLeft cr)
    ]

unsafeRows :: Either a CozoResult -> IO [[Value]]
unsafeRows (Right (CozoResult (Right (CozoOkay (NamedRows _ rs _) _)))) = pure rs
unsafeRows _ = fail "Failed to extract rows."

testWithConnection :: (Connection -> IO c) -> IO c
testWithConnection =
  bracket
    (open "mem" "" "{}" >>= either throwIO pure)
    ((() <$) . close)