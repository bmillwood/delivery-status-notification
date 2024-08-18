module Main (main) where

import Control.Monad
import Data.Maybe
import Data.String

import qualified Data.Attoparsec.Text as AP

import Email.StatusCode

testStringParser :: (String, Maybe StatusCode) -> IO ()
testStringParser (input, expected)
  | actual /= expected = error $ mconcat
      [ "parse "
      , show input
      , " expected "
      , show expected
      , " got "
      , show actual
      ]
  | otherwise =
    case expected of
      Nothing -> pure ()
      Just code
        | roundtrip == input -> pure ()
        | otherwise -> error $ mconcat
            [ "encodeString "
            , show code
            , " expected "
            , show input
            , " got "
            , show roundtrip
            ]
        where
          roundtrip = encodeString code
  where
    actual = parse input

testAttoparsec :: (String, Maybe StatusCode) -> IO ()
testAttoparsec (input, expected)
  | expected /= actual = error $ mconcat
      [ "parse "
      , show input
      , " expected "
      , show expected
      , " got "
      , show actual
      ]
  | otherwise = pure ()
  where
    actual =
      case AP.parseOnly (parser <* AP.endOfInput) (fromString input) of
        Left e
          | isNothing expected -> Nothing
          | otherwise -> error e
        Right a -> Just a

main :: IO ()
main = forM_
  [ ("5.1.1", Just (StatusCode PermanentFailure (Addressing BadDestinationMailbox)))
  , ("2.0.1", Nothing)
  , ("2.0.0", Just (StatusCode Success OtherSubjectDetail))
  , ("0.0.0", Nothing)
  , ("5.7.0", Just (StatusCode PermanentFailure (SecurityOrPolicy OtherSecurityOrPolicyDetail)))
  ]
  $ \(input, expected) -> do
    testStringParser (input, expected)
    testAttoparsec (input, expected)
