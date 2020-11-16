# simple-mailjet-client

This is merely a haskell translation of the curl specification of the mailjet API.
It only supports simple emails right now (no batch sending, file attachements, etc), but the intent is to support the entire v3.1 api.

The spec is described [here](https://dev.mailjet.com/email/guides/send-api-v31/), a simple excerpt is:

```bash
# This call sends a message to one recipient.
curl -s \
  -X POST \
  --user "$MJ_APIKEY_PUBLIC:$MJ_APIKEY_PRIVATE" \
  https://api.mailjet.com/v3.1/send \
  -H 'Content-Type: application/json' \
  -d '{
    "Messages":[
      {
        "From": {
          "Email": "pilot@mailjet.com",
          "Name": "Mailjet Pilot"
        },
        "To": [
          {
            "Email": "passenger1@mailjet.com",
            "Name": "passenger 1"
          }
        ],
        "Subject": "Your email flight plan!",
        "TextPart": "Dear passenger 1, welcome to Mailjet! May the delivery force be with you!",
        "HTMLPart": "<h3>Dear passenger 1, welcome to <a href=\"https://www.mailjet.com/\">Mailjet</a>!</h3><br />May the delivery force be with you!"
      }
    ]
  }'
```
 and the corresponding response:
````
{
  "Messages": [
    {
      "Status": "success",
      "To": [
        {
          "Email": "passenger1@mailjet.com",
          "MessageUUID": "123",
          "MessageID": 456,
          "MessageHref": "https://api.mailjet.com/v3/message/456"
        }
      ]
    }
  ]
```


