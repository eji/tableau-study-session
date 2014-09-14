
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Conduit.Binary as CB
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Aeson.Parser
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import LoadEnv
import Network.HTTP.Conduit
import Options.Applicative
import System.Environment (lookupEnv)
import System.IO(stdout)
import Web.Authenticate.OAuth

type ApiKey = B.ByteString
type ApiSecret = B.ByteString
type AccessToken = B.ByteString
type AccessTokenSecret = B.ByteString

type ScreenName = T.Text
type CreatedAt = T.Text
type Message = T.Text
type Latitude = Double
type Longitude = Double

{- オプション -}
data Options = Options { _query :: String }
  deriving (Eq, Show)

options :: Parser Options
options = Options
  <$> strOption (short 'q' <> long "query" <> metavar "QUERY" <> help "検索するタグ名")

opts :: ParserInfo Options
opts =
  info
    (helper <*> options)
    (
      fullDesc
      <> progDesc "出力フォーマット: スクリーン名 メッセージ 投稿日時 緯度 軽度"
      <> header "ハッシュタグで検索されたGeoタグ付きのツイートを取り出すためのスクリプト"
    )

main :: IO ()
main = do
  loadEnv

  api_key <- getEnv "TWITTER_OAUTH_API_KEY" toConsumerKey
  api_secret <- getEnv "TWITTER_OAUTH_API_SECRET" toConsumerSecret
  access_token <- getEnv "TWITTER_OAUTH_ACCESS_TOKEN" toAccessToken
  access_token_secret <- getEnv "TWITTER_OAUTH_ACCESS_TOKEN_SECRET" toAccessTokenSecret

  optsInfo <- execParser opts
  let query = _query optsInfo
  let oauth = makeOAuth api_key api_secret
  let credential = makeCredential access_token access_token_secret
  req <- makeRequestTofetchRecentTweetsByHashTag oauth credential query

  runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    res <- http req manager
    j <- responseBody res $$+- sinkParser json
    liftIO $ mapM_ T.putStrLn $ map valueToText $ filterGeoTaggedStatueses j

getEnv :: String -> (String -> a) -> IO a
getEnv key converter = do
  val <- lookupEnv key
  case val of
    Just v -> return $ converter v
    Nothing -> error $ "couldn't found value: " ++ key

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
makeRequestTofetchRecentTweetsByHashTag :: OAuth -> Credential -> String -> IO Request
makeRequestTofetchRecentTweetsByHashTag oauth credential htag = do 
  req <- liftIO $ parseUrl reqUrlStr
  signOAuth oauth credential req
  where
    reqUrlStr = "https://api.twitter.com/1.1/search/tweets.json?result_type=recent&count=100&q=%23" ++ htag

filterGeoTaggedStatueses :: Value -> [Value]
filterGeoTaggedStatueses (Object v) =
  case M.lookup "statuses" v of
    Nothing -> []
    Just (Array as) -> filter isGeoTaggedStatus $ V.toList as
    Just _ -> []
filterGeoTaggedStatueses _ = []

isGeoTaggedStatus :: Value -> Bool
isGeoTaggedStatus (Object v) =
  case M.lookup "geo" v of
    Nothing -> False
    Just Null -> False
    Just _ -> True
isGeoTaggedStatus _ = False

valueToText :: Value -> T.Text
valueToText val = T.intercalate (T.pack "\t") flds
  where
    flds =
      [ getScreenName val
      , getMessage val
      , getCreatedAt val
      , (T.pack . show) $ fst (getGeo val)
      , (T.pack . show) $ snd (getGeo val)
      ]
    getScreenName :: Value -> ScreenName
    getScreenName (Object v) =
      case M.lookup "user" v of
       Nothing -> ""
       Just (Object v') -> 
         case M.lookup "screen_name" v' of
           Just (String s) -> s
           Nothing -> ""
       Just _ -> ""
    getScreenName _ = ""

    getMessage :: Value -> Message
    getMessage (Object v) =
      case M.lookup "text" v of
       Nothing -> ""
       Just (String s) -> s
       Just _ -> ""
    getMessage _ = ""

    getCreatedAt :: Value -> CreatedAt
    getCreatedAt (Object v) =
      case M.lookup "created_at" v of
       Nothing -> ""
       Just (String s) -> s
       Just _ -> ""
    getCreatedAt _ = ""

    defaultGeo = (0, 0)
    getGeo :: Value -> (Latitude, Longitude)
    getGeo (Object v) =
      case M.lookup "geo" v of
       Nothing -> defaultGeo
       Just (Object v') ->
         case M.lookup "coordinates" v' of
           Nothing -> defaultGeo
           Just (Array arr) -> (getGeoNumber (arr V.! 0), getGeoNumber(arr V.! 1))
       Just _ -> defaultGeo
    getGeo _ = defaultGeo

    getGeoNumber :: Value -> Double
    getGeoNumber (Number d) = read (show d) :: Double -- FIXME
    getGeoNumber _ = 0

