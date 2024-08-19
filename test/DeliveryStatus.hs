{-# LANGUAGE OverloadedLists #-}
module DeliveryStatus (main) where

import qualified Data.Attoparsec.Text as AP

import Email.DSN.DeliveryStatus
import qualified Email.DSN.StatusCode as DSC

main :: IO ()
main = do
  let
    input = "Reporting-MTA: dns; d218-13.smtp-out.eu-west-2.amazonses.com\r\n\r\nAction: failed\r\nFinal-Recipient: rfc822; bounce@simulator.amazonses.com\r\nDiagnostic-Code: smtp; 550 5.1.1 user unknown\r\nStatus: 5.1.1\r\n\r\n"
    actual = AP.parseOnly parser input
    expected :: Either String DeliveryStatus
    expected =
      Right $ DeliveryStatus
        { perMessageFields =
            [ RawField "Reporting-MTA" " dns; d218-13.smtp-out.eu-west-2.amazonses.com"
            ]
        , perRecipientFields =
            [ [ OtherPerRecipient (RawField "Action" " failed")
              , FinalRecipient "rfc822" "bounce@simulator.amazonses.com"
              , OtherPerRecipient (RawField "Diagnostic-Code" " smtp; 550 5.1.1 user unknown")
              , Status (
                  DSC.StatusCode DSC.PermanentFailure
                    (DSC.Addressing DSC.BadDestinationMailbox)
                )
              ]
            ]
        }
  if actual == expected
  then pure ()
  else error $ mconcat
    [ "parsing "
    , show input
    , " expected "
    , show expected
    , " got "
    , show actual
    ]
