import           Giphy              (findGif)
import           System.Environment (getArgs)
import           Twilio             (sendImage)


twilioAccountSid = "YOUR_TWILIO_ACCOUNT_SID"
twilioAuthToken = "YOUR_TWILIO_AUTH_SID"
twilioFromNumber = "YOUR_TWILIO_FROM_NUMBER"
twilioToNumber = "YOUR_TWILIO_TO_NUMBER"

giphyApiKey = "YOUR_GIPHY_API_KEY"
giphyMaxByteSize = 5000000


sendGif :: String -> IO Bool
sendGif = sendImage twilioAccountSid twilioAuthToken
                    twilioFromNumber twilioToNumber

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [] -> putStrLn "Provide a search query argument"
    query : _  -> do
      result <- findGif giphyApiKey giphyMaxByteSize query
      case result of
        Left err -> putStrLn err
        Right url -> do
          result <- sendGif url
          if result then
            putStrLn "Successfully sent gif"
          else
            putStrLn "Error sending gif"
