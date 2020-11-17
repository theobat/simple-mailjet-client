{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailjet.Config where

import Data.String (IsString)
import Network.HTTP.Req (Scheme(Https), Url, (/:), https)
import Data.ByteString (ByteString)

data MailjetConfigRecord = MailjetConfigRecord {
  mjApikeyPublic :: ByteString,
  mjApikeyPrivate :: ByteString
} deriving (Show)

fromTuple :: (ByteString, ByteString) -> MailjetConfigRecord
fromTuple = uncurry MailjetConfigRecord

apiVersion :: (IsString a) => a
apiVersion = "v3.1"

-- | The api URL.
apiURL :: (IsString a, Semigroup a) => a
apiURL = "https://api.mailjet.com/" <> apiVersion <> "/send"

apiReqURL :: Url 'Https
apiReqURL = https "api.mailjet.com" /: apiVersion /: "send"
