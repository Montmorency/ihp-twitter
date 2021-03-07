module TwitterAPI.Config where

import Prelude (print)

endpoint :: String
endpoint = "https://api.twitter.com/2/"

data TwitterKeys = TwitterKeys { 
                                 _apiKey :: ByteString
                               , _apiSecretKey :: ByteString
                               , _bearerToken :: ByteString
                               }

