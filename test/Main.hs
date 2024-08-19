module Main (main) where

import qualified DeliveryStatus
import qualified StatusCode

main :: IO ()
main = do
  StatusCode.main
  DeliveryStatus.main
