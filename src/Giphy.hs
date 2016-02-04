{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Giphy (findGif) where
  import           Data.Aeson           (FromJSON (parseJSON), Value (Object),
                                         eitherDecode, (.:), withObject)
  import           Network.HTTP.Conduit (simpleHttp)
  import           Data.List            (find)


  data SearchResponse = SearchResponse { results :: [SearchResult] }
  instance FromJSON SearchResponse where
    parseJSON = withObject "SearchResponse" $ \object -> do
      results <- object .: "data"
      return SearchResponse{..}

  data SearchResult = SearchResult { url :: String, bytes :: Integer }
  instance FromJSON SearchResult where
    parseJSON = withObject "SearchResult" $ \object -> do
      images <- object .: "images"
      original <- images .: "original"
      size <- original .: "size"
      let bytes = read size
      url <- original .: "url"
      return SearchResult{..}

  getSearchUrl :: String -> String -> String
  getSearchUrl apiKey query = "http://api.giphy.com/v1/gifs/search?api_key="
                              ++ apiKey
                              ++ "&q="
                              ++ query

  findGif :: String -> Integer -> String -> IO (Either String String)
  findGif apiKey maxSize query = do
    let queryUrl = getSearchUrl apiKey query
    response <- eitherDecode <$> simpleHttp queryUrl
    case response of
      Left err                             -> return $ Left err
      Right (SearchResponse searchResults) -> case searchResults of
        []      -> return $ Left "No results found for query"
        results -> do
          let match = find (\r -> bytes r < maxSize) results
          case match of
            Nothing     -> return $ Left "No results found for query"
            Just result -> return $ Right . url $ result
