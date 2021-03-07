#!/usr/bin/env run-script
module Application.Script.GetFilterStream where
 
import Application.Script.Prelude 
import Prelude (print)

import           Data.Aeson ( encode
                            , object
                            , (.=)
                            )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.ByteString

import           Data.Conduit ( (.|)
                              , mapMC
                              , awaitForever
                              , runConduit
                              )

import           Data.Conduit.Attoparsec ( sinkParser )
import qualified Data.Conduit.List as CL

import           Control.Lens hiding ( (.=) )

import           Network.HTTP.Client ( defaultManagerSettings
                                     , newManager
                                     , httpLbs
                                     )

import           Network.HTTP.Conduit ( http )

import           Network.HTTP.Client.TLS ( tlsManagerSettings )

import           Network.HTTP.Simple  ( httpBS
                                      , httpLBS
                                      , httpJSON
                                      , getResponseBody
                                      , setRequestHeader
                                      , setRequestBodyJSON
                                      , parseRequest
                                      , addRequestHeader
                                      , Request
                                      )
 

--import qualified Web.Authenticate.OAuth as OA
import Web.Twitter.Types (User(..), Status(..))

data Tweet = Tweet {author_id :: Int , created_at :: UTCTime}

endpoint :: String
endpoint = "https://api.twitter.com/2/"

data TwitterKeys = TwitterKeys { 
                                 _apiKey :: ByteString
                               , _apiSecretKey :: ByteString
                               , _bearerToken :: ByteString
                               }

--maybe 
setTweetRequestHeader :: Request -> Request
setTweetRequestHeader =  ( addRequestHeader "Content-Type" "application/json"
                         . setRequestHeader "Authorization" [("Bearer " <> (_bearerToken twitterKeys))]
                         )

--set JSON rules up to 25 rules permitted per stream.
postStreamRulesRequest :: IO (Request)
postStreamRulesRequest = do
                    let rule_1 = object ["value" .= ("#ihp @digitallyinduce #IHP" :: String), "tag" .= ("digitallyinduced" :: String)]
                        rule_2 = object ["value" .= ("#tesla SpaceX Tesla" :: String), "tag" .= ("tesla/spacex data" :: String)]
                        rules = object["add" .= [rule_1, rule_2]]

                    request <- parseRequest ("POST " <> endpoint <> "tweets/search/stream/rules")
                    pure $ ((setRequestBodyJSON rules) . setTweetRequestHeader) request
                    
getStreamRulesRequest :: IO (Request) 
getStreamRulesRequest = do
                          request <- parseRequest ("GET " <> endpoint <> "tweets/search/stream/rules")
                          pure $ setTweetRequestHeader request

deleteStreamRulesRequest :: IO (Request)
deleteStreamRulesRequest = do
                             let delete_rules = object ["delete" .= object["ids" .= ["" :: String]]]
                             request <- parseRequest ("POST " <> endpoint <> "stream/rules")
                             pure $ ((setRequestBodyJSON delete_rules) . setTweetRequestHeader) request

getFilterStreamRequest :: IO (Request)
getFilterStreamRequest =  do 
                      request <- parseRequest ("GET " <> endpoint <> "tweets/search/stream?tweet.fields=created_at&expansions=author_id")
                      pure $ setTweetRequestHeader request

run :: Script
run = do
    manager <- newManager tlsManagerSettings
    request <- getTweetStreamRequest
    runResourceT $ do
        response <- http request manager
        runConduit  
            .| awaitForever (responseBody response)
            .| mapM_C print -- .| streamIntoDB

