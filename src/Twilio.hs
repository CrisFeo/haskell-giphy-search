{-# LANGUAGE OverloadedStrings #-}

module Twilio (sendImage) where
  import           Network.HTTP.Conduit
  import           Network.HTTP.Types
  import qualified Data.ByteString.Char8 as B
  import           Base64 (encode)
  import           Control.Arrow


  getMessageEndpoint :: String -> String
  getMessageEndpoint accountSid =
    "https://api.twilio.com/2010-04-01/Accounts/"
    ++ accountSid
    ++ "/Messages.json"

  createMessageParameters ::
    String -> String -> String -> [(B.ByteString, B.ByteString)]
  createMessageParameters fromNumber toNumber url = map (B.pack *** B.pack)
                                                        [("To", toNumber),
                                                        ("From", fromNumber),
                                                        ("MediaUrl", url)]

  getTwilioAuthHeader :: String -> String -> Header
  getTwilioAuthHeader accountSid authToken =
    (hAuthorization, B.pack . unwords $ ["Basic", encode credentials])
    where credentials = accountSid ++ ":" ++ authToken

  buildMessageRequest ::
    String -> String -> String -> String -> String -> Request -> Request
  buildMessageRequest accountSid authToken fromNumber toNumber url request =
    urlEncodedRequest {requestHeaders = authHeader : headers}
    where twilioParameters = createMessageParameters fromNumber toNumber url
          urlEncodedRequest = urlEncodedBody twilioParameters request
          headers = requestHeaders urlEncodedRequest
          authHeader = getTwilioAuthHeader accountSid authToken

  sendImage :: String -> String -> String -> String -> String -> IO Bool
  sendImage accountSid authToken fromNumber toNumber url = do
    baseRequest <- parseUrl $ getMessageEndpoint accountSid
    let twilioRequest = buildMessageRequest accountSid authToken
                                            fromNumber toNumber url
                                            baseRequest
    manager <- newManager tlsManagerSettings
    response <- httpLbs twilioRequest manager
    case responseStatus response of
      Status 201 _ -> return True
      _            -> return False
