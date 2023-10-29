{-# LANGUAGE LambdaCase #-}

module Database.Cozo.FromRelation (
  -- * FromRelation
  FromRelation (..),

  -- ** RelationParser
  RelationParser,
  RelationParserError (..),
  parseRelation,
  fromValue,
  value,

  -- * Re-exports
  FromJSON (..),
  fromJSON,
  Value (..),
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Exception (Exception)
import Control.Monad (MonadPlus, ap)
import Data.Aeson (FromJSON (..), Result (..), Value (..), fromJSON)
import Data.Functor.Identity
import GHC.Generics (Generic)

{- |
A class for marshalling types from JSON values returned by the "rows" field
of a Cozo result.
-}
class FromRelation r where
  fromRelation :: RelationParser r

instance (FromJSON a) => FromRelation (Identity a) where
  fromRelation :: (FromJSON a) => RelationParser (Identity a)
  fromRelation = Identity <$> fromValue

instance (FromJSON a, FromJSON b) => FromRelation (a, b) where
  fromRelation :: (FromJSON a, FromJSON b) => RelationParser (a, b)
  fromRelation = (,) <$> fromValue <*> fromValue

data RelationParserError
  = NoMoreValuesToConsume
  | NotAllValuesConsumed [Value]
  | RelationParserError String
  deriving (Show, Eq, Generic)

instance Exception RelationParserError

newtype RelationParser a = RelationParser
  { runRelationParser ::
      [Value] ->
      (Either RelationParserError a, [Value])
  }
  deriving (Functor, Generic)

instance Applicative RelationParser where
  pure :: a -> RelationParser a
  pure x = RelationParser (Right x,)
  (<*>) :: RelationParser (a -> b) -> RelationParser a -> RelationParser b
  (<*>) = ap

instance Monad RelationParser where
  (>>=) :: RelationParser a -> (a -> RelationParser b) -> RelationParser b
  (RelationParser p) >>= f = RelationParser $ \s ->
    let (res, s') = p s
     in case res of
          Left e -> (Left e, s')
          Right x ->
            case f x of
              RelationParser sf -> sf s'

instance Alternative RelationParser where
  empty :: RelationParser a
  empty = RelationParser (Left . RelationParserError $ "Failed while parsing relation.",)
  (<|>) :: RelationParser a -> RelationParser a -> RelationParser a
  (RelationParser f) <|> (RelationParser g) = RelationParser $ \s ->
    case f s of
      fr@(r, _) ->
        case r of
          Left _ -> g s
          Right _ -> fr

instance MonadFail RelationParser where
  fail :: String -> RelationParser a
  fail s = RelationParser (Left . RelationParserError $ s,)

instance MonadPlus RelationParser

{- |
Run a `RelationParser'.

This will produce a left value if

  - Any values failed to be parsed.
  - More values were consumed than existed.
  - Not every value was consumed.
-}
parseRelation :: RelationParser a -> [Value] -> Either RelationParserError a
parseRelation (RelationParser p) vs =
  case p vs of
    (r, []) -> r
    (e, remaining) -> const (Left . NotAllValuesConsumed $ remaining) =<< e

{- |
Pop the next value from the relation.
-}
value :: RelationParser Value
value = RelationParser
  $ \case
    [] -> (Left NoMoreValuesToConsume, [])
    x : xs -> (Right x, xs)

{- |
Pop a value and interpret it from its JSON representation.
-}
fromValue :: (FromJSON a) => RelationParser a
fromValue = value >>= fromResult . fromJSON

fromResult :: Result a -> RelationParser a
fromResult r = RelationParser
  $ case r of
    Error s -> (Left . RelationParserError $ s,)
    Success a -> (Right a,)