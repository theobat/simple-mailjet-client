{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Mailjet.SendAPI where

import Data.Aeson (FromJSON (parseJSON), Options, ToJSON (toEncoding, toJSON), defaultOptions, encode, fieldLabelModifier, genericParseJSON, genericToEncoding, genericToJSON, object, omitNothingFields, withObject, (.:), (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Mailjet.Config (MailjetConfigRecord, apiReqURL, mjApikeyPrivate, mjApikeyPublic)
import Network.HTTP.Req
  ( POST (POST),
    ReqBodyBs (ReqBodyBs),
    basicAuth,
    defaultHttpConfig,
    jsonResponse,
    req,
    responseBody,
    runReq,
  )

-- | Email is forst, name is second. Name is mandatory
-- but you can put name as email or as blank.
data EmailAndName = EmailAndName Text Text deriving (Eq)

instance ToJSON EmailAndName where
  toJSON (EmailAndName emailV nameV) =
    object ["Email" .= emailV, "Name" .= nameV]

-- | This is an object used both for sending and receiving a list of messages.
-- Both sender and recevier JSON are expected to be like:
-- @
-- { Messages: [...] }
-- @
-- Hence this object.
newtype MailjetWrapper a = MailjetWrapper [a] deriving (Show)

instance ToJSON a => ToJSON (MailjetWrapper a) where
  toJSON (MailjetWrapper messageList) =
    object ["Messages" .= toJSON messageList]

instance FromJSON a => FromJSON (MailjetWrapper a) where
  parseJSON = withObject "MailjetWrapper" parser
    where
      parser obj = do
        res <- obj .: "Messages"
        MailjetWrapper <$> parseJSON res

-- | All fields are prefixed @mjm@. This is a requirement for a valid generic toJSON.
-- All the parts after mjm should match the specification in <https://dev.mailjet.com/email/guides/send-api-v31/ here> .
data MailjetMail = MailjetMail
  { -- | The (claimed) authors of the email. Note that the real sender is still
    -- mailjet no matter what.
    mjmFrom :: EmailAndName,
    -- | A list of recipient.
    mjmTo :: [EmailAndName],
    -- | A list of recipient in copy.
    mjmCc :: [EmailAndName],
    -- | A list of hidden recipients in copy.
    mjmBcc :: [EmailAndName],
    mjmSubject :: Text,
    -- | If nothing is given for the HTML field, the text part is used for both.
    mjmHTMLPart :: Maybe Text,
    mjmTextPart :: Text
  }
  deriving (Generic)

-- |  Single recipient, no html.
simpleMail :: EmailAndName -> EmailAndName -> Text -> Text -> MailjetMail
simpleMail fromVal toVal subjectVal contentVal =
  MailjetMail
    { mjmFrom = fromVal,
      mjmTo = [toVal],
      mjmCc = [],
      mjmBcc = [],
      mjmSubject = subjectVal,
      mjmHTMLPart = Just contentVal,
      mjmTextPart = contentVal
    }

emailJSONOption :: Options
emailJSONOption =
  defaultOptions
    { fieldLabelModifier = dropLibPrefix,
      omitNothingFields = True
    }

instance ToJSON MailjetMail where
  toJSON = genericToJSON emailJSONOption
  toEncoding = genericToEncoding emailJSONOption

dropLibPrefix = drop (Text.length "mjm")

-- | The result is either a success or an error.
--
-- @
-- {
--   "Messages": [
--     {
--       "Errors": [
--         {
--           "ErrorIdentifier": "88b5ca9f-5f1f-42e7-a45e-9ecbad0c285e",
--           "ErrorCode": "send-0003",
--           "StatusCode": 400,
--           "ErrorMessage": "At least \"HTMLPart\", \"TextPart\" or \"TemplateID\" must be provided.",
--           "ErrorRelatedTo": ["HTMLPart", "TextPart"]
--         }
--       ],
--       "Status": "error"
--     },
--     {
--       "Status": "success",
--       "CustomID": "",
--       "To": [
--         {
--           "Email": "passenger2@mailjet.com",
--           "MessageUUID": "cb927469-36fd-4c02-bce4-0d199929a207",
--           "MessageID": 70650219165027410,
--           "MessageHref": "https://api.mailjet.com/v3/REST/message/70650219165027410"
--         }
--       ],
--       "Cc": [],
--       "Bcc": []
--     }
--   ]
-- }
-- @
data MessageResult
  = Success [MailjetToSuccess]
  | Error [MailjetError]
  deriving (Eq, Show)

instance FromJSON MessageResult where
  parseJSON = withObject "MessageResult" parser
    where
      parser obj = do
        statusVal <- obj .: "Status"
        case statusVal of
          "success" -> do
            resultList <- obj .: "To"
            Success <$> parseJSON @[MailjetToSuccess] resultList
          "error" -> do
            errorList <- obj .: "Errors"
            Error <$> parseJSON @[MailjetError] errorList
          v -> fail $ "Unexpected value in message status = " <> v

data MailjetToSuccess = MailjetToSuccess
  { mjmEmail :: Maybe Text,
    mjmMessageUUID :: Maybe Text,
    mjmMessageID :: Maybe Int64,
    mjmMessageHref :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON MailjetToSuccess where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropLibPrefix}

data MailjetError = MailjetError
  { mjmErrorIdentifier :: Maybe Text,
    mjmErrorCode :: Maybe Text,
    mjmStatusCode :: Maybe Int,
    mjmErrorMessage :: Maybe Text,
    mjmErrorRelatedTo :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON MailjetError where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropLibPrefix}

-- | The main way to send emails through mailjet.
sendMailList :: MailjetConfigRecord -> [MailjetMail] -> IO (MailjetWrapper MessageResult)
sendMailList config emailValueList = do
  let jsonBody = encode $ toJSON $ MailjetWrapper emailValueList
  let requestBody = ReqBodyBs $ toStrict jsonBody
  let options = basicAuth (mjApikeyPublic config) (mjApikeyPrivate config)
  finalRes <-
    runReq defaultHttpConfig $
      req
        POST -- method
        apiReqURL
        requestBody -- use built-in options or add your own
        (jsonResponse @(MailjetWrapper MessageResult)) -- specify how to interpret response
        options -- query params, headers, explicit port number, etc.
  pure $ responseBody finalRes
