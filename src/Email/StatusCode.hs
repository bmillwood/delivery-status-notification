-- | https://www.rfc-editor.org/rfc/rfc3463
module Email.StatusCode where

import GHC.Generics (Generic)

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
  deriving (Generic, Enum, Eq, Ord, Read, Show)

data MailboxDetail
  = OtherMailboxDetail
  | MailboxDisabled
  | MailboxFull
  | MessageLengthExceedsAdminLimit
  | MailingListExpansionProblem
  deriving (Generic, Enum, Eq, Ord, Read, Show)

data SystemDetail
  = OtherSystemDetail
  | MailSystemFull
  | SystemNotAcceptingMessages
  | SystemNotCapableOfSelectedFeatures
  | MessageTooBigForSystem
  | SystemIncorrectlyConfigured
  deriving (Generic, Enum, Eq, Ord, Read, Show)

data NetworkAndRoutingDetail
  = OtherNetworkAndRoutingDetail
  | NoAnswerFromHost
  | BadConnection
  | DirectoryServerFailure
  | UnableToRoute
  | MailSystemCongestion
  | RoutingLoopDetected
  | DeliveryTimeExpired
  deriving (Generic, Enum, Eq, Ord, Read, Show)

data DeliveryProtocolDetail
  = OtherDeliveryProtocolDetail
  | InvalidCommand
  | SyntaxError
  | TooManyRecipients
  | InvalidCommandArguments
  | WrongProtocolVersion
  deriving (Generic, Enum, Eq, Ord, Read, Show)

data MessageContentOrMediaDetail
  = OtherContentOrMediaDetail
  | MediaNotSupported
  | ConversionRequiredAndProhibited
  | ConversionRequiredAndUnsupported
  | ConversionWithLossPerformed
  | ConversionFailed
  deriving (Generic, Enum, Eq, Ord, Read, Show)

data SecurityOrPolicyDetail
  = OtherSecurityOrPolicyDetail
  | DeliveryNotAuthorized
  | MailingListExpansionProhibited
  | SecurityConversionRequiredButNotPossible
  | SecurityFeaturesNotSupported
  | CryptographicFailure
  | CryptographicAlgorithmNotSupported
  | MessageIntegrityFailure
  deriving (Generic, Enum, Eq, Ord, Read, Show)

data SubjectDetail
  = OtherSubjectDetail
  | Addressing AddressingDetail
  | Mailbox MailboxDetail
  | MailSystem SystemDetail
  | NetworkAndRouting NetworkAndRoutingDetail
  | MailDeliveryProtocol DeliveryProtocolDetail
  | MessageContentOrMedia MessageContentOrMediaDetail
  | SecurityOrPolicy SecurityOrPolicyDetail
  deriving (Generic, Eq, Ord, Read, Show)
