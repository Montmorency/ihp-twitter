module Application.Script.TweetConduit where

import Application.Script.Prelude

endpoint :: String
endpoint = "https://api.twitter.com/2/"

type URL = String
type TweetId = Int
type Username = String

--twitter v2 API
--GET Single Tweet
--https://api.twitter.com/2/tweets/:id
--  https://api.twitter.com/2/tweets/:id
getSingleTweetURL :: TweetId -> URL
getSingleTweetURL tweetId = endpoint <> "tweets/" <> tweetId

-- https://api.twitter.com/2/tweets?ids=
getMultipleTweetsURL :: [TweetId] -> URL
getMultipleTweetsURL tweetIds = endpoint <> "tweets?ids" <> (intercalate ":" tweetIds)

-- [User Lookup](https://documenter.getpostman.com/view/9956214/T1LMiT5U#7358bba6-04d0-4edf-adda-309a900e0569)
-- https://api.twitter.com/2/users/:id
getUserByIDURL :: [UserId] -> URL
getUserByIDURL userIds = endpoint <> "users" <> (intercalate ":" userIds)

-- https://api.twitter.com/2/users?ids=
getMultipleUsersByIDURL :: [UserIds] -> URL
getMultipleUsersByIDURL userIds = endpoint <> "users?ids" <> (intercalate ":" userIds)

-- https://api.twitter.com/2/users/by/username/:username
getUserByUsernameURL :: Username -> URL
getUserByUsernameURL username = endpoint <> "users/by/username/" <> username 

-- https://api.twitter.com/2/users/by?usernames=
getMultipleUsersByUsernameURL :: [Username] -> URL
getMultipleUsersByUsernameURL usernames= endpoint <> "users/by/username/" <> (intercalate ":" usernames) 

-- https://api.twitter.com/2/tweets/search/recent?query=
-- e.g. curl --location --request GET 'https://api.twitter.com/2/tweets/search/recent?query=nyc&tweet.fields=author_id,created_at,entities,geo,in_reply_to_user_id,lang,possibly_sensitive,referenced_tweets,source'

getRecentSearchURL :: URL
getRecentSearchURL = endpoint <> "tweets/search/recent"

-- [FilteredStreams](https://documenter.getpostman.com/view/9956214/T1LMiT5U)
-- https://api.twitter.com/2/tweets/search/stream/rules
postStreamAddRules ::  Manager -> Rules -> Request
postStreamAddRules manager rules =  do
                      setRequestBodyFile rules 
                    $ setRequestHeader "Authorization" ["Bearer " <> (manager ^. _bearerToken)]
                    $ setRequestHeader "Content-Type" ["application/json"]
                    $ ("POST " <> endpoint <> "stream/rules")

getStreamURL :: URL
getStreamURL = endpoint <> "tweets/search/stream" -- ?tweet.fields=created_at&expansions=author_id&user.fields=created_at"

-- https://api.twitter.com/2/tweets/search/stream/rules
-- getRetrieveRules ::
-- {"delete": {"ids": [""]}}
-- postDeleteRules ::
--Hide replies
