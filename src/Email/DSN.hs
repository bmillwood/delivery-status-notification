-- | https://www.rfc-editor.org/rfc/rfc3464
module Email.DSN
  ( DeliveryStatus(..), PerRecipientField(..), RawField(..)
  , deliveryStatusParser
  ) where

import Control.Applicative
import Data.Char
import Data.List.NonEmpty (NonEmpty, some1)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Attoparsec.Text as AP

import qualified Email.StatusCode as ESC

data RawField = RawField { fieldName :: Text, fieldBody :: Text }
  deriving (Generic, Eq, Ord, Read, Show)

-- Parsed according to https://www.rfc-editor.org/rfc/rfc5322.html#section-3.6.8
-- In RFC3464 this syntax is defined by reference to RFC822, but RFC5322 updates
-- RFC822 and I found it easier to implement.
fieldParser :: AP.Parser RawField
fieldParser =
  RawField
  <$> AP.takeWhile1 (\c -> c > ' ' && c /= ':' && c < '\x80')
  <* skipLWS <* AP.char ':'
  <*> unstructured
  <* AP.string "\r\n"

skipLWS :: AP.Parser ()
skipLWS =
  AP.skipWhile isWsp
  <* AP.option () (AP.string "\r\n" *> AP.skip isWsp *> skipLWS)

{-
   unstructured    =   (*([FWS] VCHAR) *WSP) / obs-unstruct
    VCHAR          =  %x21-7E ; visible (printing) characters
    WSP            = SP / HTAB ; white space
    SP             = %x20
    HTAB           = %x09
   FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS ; Folding white space
   obs-FWS         =   1*([CRLF] WSP)
   obs-unstruct    =   *( (*CR 1*(obs-utext / FWS)) / 1*LF ) *CR
   obs-utext       =   %d0 / obs-NO-WS-CTL / VCHAR
   obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
                       %d11 /             ;  characters that do not
                       %d12 /             ;  include the carriage
                       %d14-31 /          ;  return, line feed, and
                       %d127              ;  white space characters
-}

-- Omitting the obsolete syntax for simplicity, even though that is of course
-- incorrect in principle.
unstructured :: AP.Parser Text
unstructured =
  Text.append
    <$> (Text.concat <$> many (Text.snoc <$> fws0 <*> AP.satisfy isVchar))
    <*> AP.takeWhile isWsp
  -- <|> obsUnstruct

fws0 :: AP.Parser Text
fws0 =
  Text.append
    <$> AP.takeWhile isWsp
    <*> AP.option "" (AP.string "\r\n" *> AP.takeWhile1 isWsp)
  -- <|> obsFws

isWsp :: Char -> Bool
isWsp c = c == ' ' || c == '\t'

isVchar :: Char -> Bool
isVchar c = c >= '\x21' && c <= '\x7e'

-- | A subset of the RFC-specified per-recipient fields, chosen because they are
-- the ones most useful for interpreting bounces.
data PerRecipientField
  = FinalRecipient { addressType :: Text, address :: Text }
  | Status ESC.StatusCode
  | OtherPerRecipient RawField
  deriving (Generic, Eq, Ord, Read, Show)

perRecipientFieldParser :: AP.Parser PerRecipientField
perRecipientFieldParser = ofRaw =<< fieldParser
  where
    ofRaw raw@RawField{ fieldName, fieldBody } =
      case Text.toLower fieldName of
        "final-recipient" ->
          case Text.break (== ';') fieldBody of
            (_, "") -> fail "final-recipient should contain ;"
            (addressType, rest) ->
              pure $ FinalRecipient
                { addressType = Text.dropWhile isSpace addressType
                , address = Text.dropWhile isSpace $ Text.drop 1 rest
                }
        "status" ->
          either fail (pure . Status)
          $ AP.parseOnly (ESC.parser <* AP.endOfInput)
          $ Text.dropWhile isSpace fieldBody
        _ -> pure $ OtherPerRecipient raw

-- | Body of a message/delivery-status
-- https://www.rfc-editor.org/rfc/rfc3464#section-2.1
data DeliveryStatus = DeliveryStatus
  { perMessageFields :: NonEmpty RawField
  , perRecipientFields :: NonEmpty (NonEmpty PerRecipientField)
  } deriving (Generic, Eq, Ord, Read, Show)

-- "Reporting-MTA: dns; d218-13.smtp-out.eu-west-2.amazonses.com\r\n\r\nAction: failed\r\nFinal-Recipient: rfc822; bounce@simulator.amazonses.com\r\nDiagnostic-Code: smtp; 550 5.1.1 user unknown\r\nStatus: 5.1.1\r\n\r\n"

-- | This parser isn't fully compliant with the RFC, since it omits the syntax
-- variants marked as obsolete. The obsolete syntax may be supported in future,
-- but was omitted for simplicity (my use cases didn't need it).
deliveryStatusParser :: AP.Parser DeliveryStatus
deliveryStatusParser =
  DeliveryStatus
    <$> some1 fieldParser
    <*> some1 (AP.string "\r\n" *> some1 perRecipientFieldParser)
