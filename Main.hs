
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import LoadEnv
import Network.HTTP.Conduit
import System.Environment (lookupEnv)
import System.IO(stdout)
import Web.Authenticate.OAuth

main :: IO ()
main = do
  loadEnv

  api_key <- getEnv "TWITTER_OAUTH_API_KEY" toConsumerKey
  api_secret <- getEnv "TWITTER_OAUTH_API_SECRET" toConsumerSecret
  access_token <- getEnv "TWITTER_OAUTH_ACCESS_TOKEN" toAccessToken
  access_token_secret <- getEnv "TWITTER_OAUTH_ACCESS_TOKEN_SECRET" toAccessTokenSecret

  let oauth = makeOAuth api_key api_secret
  let credential = makeCredential access_token access_token_secret
  req <- makeRequestTofetchRecentTweetsByHashTag oauth credential "hoge"

  runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    res <- http req manager
    -- @see http://www.yesodweb.com/blog/2012/07/resumablesource
    responseBody res $$+- CB.sinkHandle stdout

getEnv :: String -> (String -> a) -> IO a
getEnv key converter = do
  val <- lookupEnv key
  case val of
    Just v -> return $ converter v
    Nothing -> error $ "couldn't found value: " ++ key

type ApiKey = B.ByteString
type ApiSecret = B.ByteString
type AccessToken = B.ByteString
type AccessTokenSecret = B.ByteString

toConsumerKey :: String -> ApiKey
toConsumerKey = B8.pack

toConsumerSecret :: String -> ApiSecret
toConsumerSecret = B8.pack

toAccessToken :: String ->AccessToken
toAccessToken = B8.pack

toAccessTokenSecret :: String ->AccessToken
toAccessTokenSecret = B8.pack

makeOAuth :: ApiKey -> ApiSecret -> OAuth
makeOAuth api_key api_secret =
  newOAuth {
    oauthRequestUri = "https://api.twitter.com/oauth/request_token",
    oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token",
    oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize",
    oauthConsumerKey = api_key,
    oauthConsumerSecret = api_secret
  }
  
makeCredential :: AccessToken -> AccessTokenSecret -> Credential
makeCredential access_token access_token_secret = newCredential access_token access_token_secret

{- Twitter API requests -}
--fetchRecentTweetsByHashTag :: TwitterHashTag -> IO(TwitterFetchResult)
--fetchRecentTweetsByHashTag = do

makeRequestTofetchRecentTweetsByHashTag :: OAuth -> Credential -> String -> IO Request
makeRequestTofetchRecentTweetsByHashTag oauth credential htag = do 
  req <- liftIO $ parseUrl reqUrlStr
  signOAuth oauth credential req
  where
    reqUrlStr = "https://api.twitter.com/1.1/search/tweets.json?result_type=recent&q=%23" ++ htag
