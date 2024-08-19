-- | https://www.rfc-editor.org/rfc/rfc3463
module Email.DSN.StatusCode
  ( StatusCode(..)
  , Class(..), encodeClass, decodeClass
  , SubjectDetail(..), encodeSubjectDetail, decodeSubjectDetail
  , AddressingDetail(..), encodeAddressingDetail, decodeAddressingDetail
  , MailboxDetail(..), encodeMailboxDetail, decodeMailboxDetail
  , MailSystemDetail(..), encodeMailSystemDetail, decodeMailSystemDetail
  , NetworkAndRoutingDetail(..), encodeNetworkAndRoutingDetail, decodeNetworkAndRoutingDetail
  , DeliveryProtocolDetail(..), encodeDeliveryProtocolDetail, decodeDeliveryProtocolDetail
  , MessageContentOrMediaDetail(..), encodeMessageContentOrMediaDetail, decodeMessageContentOrMediaDetail
  , SecurityOrPolicyDetail(..), encodeSecurityOrPolicyDetail, decodeSecurityOrPolicyDetail
  , parse, parseWithRemainder
  , encodeString
  , parser
  ) where

import Data.Char (isDigit, isSpace)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import qualified Data.Attoparsec.Text as AP

data StatusCode = StatusCode Class SubjectDetail
  deriving (Generic, Eq, Ord, Read, Show)

data Class
  = Success
  | PersistentTransientFailure
  | PermanentFailure
  deriving (Generic, Eq, Ord, Read, Show)

encodeClass :: Class -> Int
encodeClass c = case c of
  Success -> 2
  PersistentTransientFailure -> 4
  PermanentFailure -> 5

decodeClass :: Int -> Maybe Class
decodeClass c = case c of
  2 -> Just Success
  4 -> Just PersistentTransientFailure
  5 -> Just PermanentFailure
  _ -> Nothing

safeToEnum :: forall a. (Bounded a, Enum a) => Int -> Maybe a
safeToEnum i
  | i < fromEnum (minBound :: a)
    || i > fromEnum (maxBound :: a) = Nothing
  | otherwise = Just (toEnum i)

data AddressingDetail
  = OtherAddressDetail
  | BadDestinationMailbox
  | BadDestinationSystem
  | BadDestinationMailboxSyntax
  | DestinationMailboxAmbiguous
  | DestinationAddressValid
  | DestinationMailboxMovedNoForwarding
  | BadSenderMailboxSyntax
  | BadSenderSystemAddress
  deriving (Generic, Bounded, Enum, Eq, Ord, Read, Show)

encodeAddressingDetail :: AddressingDetail -> Int
encodeAddressingDetail = fromEnum

decodeAddressingDetail :: Int -> Maybe AddressingDetail
decodeAddressingDetail = safeToEnum

data MailboxDetail
  = OtherMailboxDetail
  | MailboxDisabled
  | MailboxFull
  | MessageLengthExceedsAdminLimit
  | MailingListExpansionProblem
  deriving (Generic, Bounded, Enum, Eq, Ord, Read, Show)

encodeMailboxDetail :: MailboxDetail -> Int
encodeMailboxDetail = fromEnum

decodeMailboxDetail :: Int -> Maybe MailboxDetail
decodeMailboxDetail = safeToEnum

data MailSystemDetail
  = OtherMailSystemDetail
  | MailSystemFull
  | SystemNotAcceptingMessages
  | SystemNotCapableOfSelectedFeatures
  | MessageTooBigForSystem
  | SystemIncorrectlyConfigured
  deriving (Generic, Bounded, Enum, Eq, Ord, Read, Show)

encodeMailSystemDetail :: MailSystemDetail -> Int
encodeMailSystemDetail = fromEnum

decodeMailSystemDetail :: Int -> Maybe MailSystemDetail
decodeMailSystemDetail = safeToEnum

data NetworkAndRoutingDetail
  = OtherNetworkAndRoutingDetail
  | NoAnswerFromHost
  | BadConnection
  | DirectoryServerFailure
  | UnableToRoute
  | MailSystemCongestion
  | RoutingLoopDetected
  | DeliveryTimeExpired
  deriving (Generic, Bounded, Enum, Eq, Ord, Read, Show)

encodeNetworkAndRoutingDetail :: NetworkAndRoutingDetail -> Int
encodeNetworkAndRoutingDetail = fromEnum

decodeNetworkAndRoutingDetail :: Int -> Maybe NetworkAndRoutingDetail
decodeNetworkAndRoutingDetail = safeToEnum

data DeliveryProtocolDetail
  = OtherDeliveryProtocolDetail
  | InvalidCommand
  | SyntaxError
  | TooManyRecipients
  | InvalidCommandArguments
  | WrongProtocolVersion
  deriving (Generic, Bounded, Enum, Eq, Ord, Read, Show)

encodeDeliveryProtocolDetail :: DeliveryProtocolDetail -> Int
encodeDeliveryProtocolDetail = fromEnum

decodeDeliveryProtocolDetail :: Int -> Maybe DeliveryProtocolDetail
decodeDeliveryProtocolDetail = safeToEnum

data MessageContentOrMediaDetail
  = OtherContentOrMediaDetail
  | MediaNotSupported
  | ConversionRequiredAndProhibited
  | ConversionRequiredAndUnsupported
  | ConversionWithLossPerformed
  | ConversionFailed
  deriving (Generic, Bounded, Enum, Eq, Ord, Read, Show)

encodeMessageContentOrMediaDetail :: MessageContentOrMediaDetail -> Int
encodeMessageContentOrMediaDetail = fromEnum

decodeMessageContentOrMediaDetail :: Int -> Maybe MessageContentOrMediaDetail
decodeMessageContentOrMediaDetail = safeToEnum

data SecurityOrPolicyDetail
  = OtherSecurityOrPolicyDetail
  | DeliveryNotAuthorized
  | MailingListExpansionProhibited
  | SecurityConversionRequiredButNotPossible
  | SecurityFeaturesNotSupported
  | CryptographicFailure
  | CryptographicAlgorithmNotSupported
  | MessageIntegrityFailure
  deriving (Generic, Bounded, Enum, Eq, Ord, Read, Show)

encodeSecurityOrPolicyDetail :: SecurityOrPolicyDetail -> Int
encodeSecurityOrPolicyDetail = fromEnum

decodeSecurityOrPolicyDetail :: Int -> Maybe SecurityOrPolicyDetail
decodeSecurityOrPolicyDetail = safeToEnum

data SubjectDetail
  = OtherSubjectDetail
  | Addressing AddressingDetail
  | Mailbox MailboxDetail
  | MailSystem MailSystemDetail
  | NetworkAndRouting NetworkAndRoutingDetail
  | MailDeliveryProtocol DeliveryProtocolDetail
  | MessageContentOrMedia MessageContentOrMediaDetail
  | SecurityOrPolicy SecurityOrPolicyDetail
  deriving (Generic, Eq, Ord, Read, Show)

decodeSubjectDetail :: Int -> Int -> Maybe SubjectDetail
decodeSubjectDetail subjectNum detailNum =
  case subjectNum of
    0 | detailNum == 0 -> Just OtherSubjectDetail
      | otherwise -> Nothing
    1 -> Addressing <$> decodeAddressingDetail detailNum
    2 -> Mailbox <$> decodeMailboxDetail detailNum
    3 -> MailSystem <$> decodeMailSystemDetail detailNum
    4 -> NetworkAndRouting <$> decodeNetworkAndRoutingDetail detailNum
    5 -> MailDeliveryProtocol <$> decodeDeliveryProtocolDetail detailNum
    6 -> MessageContentOrMedia <$> decodeMessageContentOrMediaDetail detailNum
    7 -> SecurityOrPolicy <$> decodeSecurityOrPolicyDetail detailNum
    _ -> Nothing

encodeSubjectDetail :: SubjectDetail -> (Int, Int)
encodeSubjectDetail sd = case sd of
  OtherSubjectDetail -> (0, 0)
  Addressing d -> (1, encodeAddressingDetail d)
  Mailbox d -> (2, encodeMailboxDetail d)
  MailSystem d -> (3, encodeMailSystemDetail d)
  NetworkAndRouting d -> (4, encodeNetworkAndRoutingDetail d)
  MailDeliveryProtocol d -> (5, encodeDeliveryProtocolDetail d)
  MessageContentOrMedia d -> (6, encodeMessageContentOrMediaDetail d)
  SecurityOrPolicy d -> (7, encodeSecurityOrPolicyDetail d)

parseNums :: String -> Maybe ((Int, Int, Int), String)
parseNums inp = do
  (classNum, afterClass) <- digitDot inp
  (subjectNum, afterSubject) <- digitDot afterClass
  case span isDigit afterSubject of
    (ns, rest) -> (, rest) . (classNum, subjectNum,) <$> readMaybe ns
  where
    digitDot s = case break (== '.') s of
      (ns, _ : rest) -> (, rest) <$> readMaybe ns
      _ -> Nothing

parseWithRemainder :: String -> Maybe (StatusCode, String)
parseWithRemainder s = do
  ((classNum, subjectNum, detailNum), rest) <- parseNums s
  code <- StatusCode
    <$> decodeClass classNum
    <*> decodeSubjectDetail subjectNum detailNum
  Just (code, rest)

parse :: String -> Maybe StatusCode
parse s = do
  (code, r) <- parseWithRemainder s
  if all isSpace r
    then Just code
    else Nothing

encodeString :: StatusCode -> String
encodeString (StatusCode cl sd) =
  shows (encodeClass cl)
  . showString "."
  . shows subjectNum
  . showString "."
  $ show detailNum
  where
    (subjectNum, detailNum) = encodeSubjectDetail sd

parser :: AP.Parser StatusCode
parser =
  StatusCode
  <$> classParser
  <* AP.char '.'
  <*> sdParser
  where
    classParser =
      decodeClass <$> AP.decimal
      >>= maybe (fail "invalid class") pure
    sdParser =
      decodeSubjectDetail
        <$> AP.decimal
        <* AP.char '.'
        <*> AP.decimal
      >>= maybe (fail "invalid subject/detail") pure
