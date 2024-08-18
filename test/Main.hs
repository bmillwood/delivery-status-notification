module Main (main) where

import Control.Monad

import Email.StatusCode

main :: IO ()
main = forM_
  [ ("5.1.1", Just (StatusCode PermanentFailure (Addressing BadDestinationMailbox)))
  , ("2.0.1", Nothing)
  , ("2.0.0", Just (StatusCode Success OtherSubjectDetail))
  , ("0.0.0", Nothing)
  , ("5.7.0", Just (StatusCode PermanentFailure (SecurityOrPolicy OtherSecurityOrPolicyDetail)))
  ]
  $ \(input, expected) ->
    let
      actual = parse input
    in
    if actual == expected
    then pure ()
    else error $ mconcat
      [ "parse "
      , show input
      , " expected "
      , show expected
      , " got "
      , show actual
      ]
