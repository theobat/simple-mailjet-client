{-# LANGUAGE OverloadedStrings #-}


import Mailjet.SendAPI (EmailAndName(EmailAndName), simpleMail, sendMailList)
import Mailjet.Config (MailjetConfigRecord(MailjetConfigRecord))

main :: IO ()
main = do
  let fromV = EmailAndName "ok" "ok"
  let toV = EmailAndName "ok" "ok"
  let subject = "ok"
  let content = "ok"
  let email = simpleMail fromV toV subject content
  let secrets = MailjetConfigRecord "public" "private"
  print =<< sendMailList secrets [email]

