{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.Qiita (
  withAuthentication
  , getAnonymousRateLimit
  , getRateLimit
  , getUserInformation
  , getLoginUserInformation
  , getTagsFirstPage
  , getTagsFirstPage'
  , getTagsAFirstPage
  , getTagsAFirstPage'
  , QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  , Tag(..)
  , QiitaContext(..)
  , Pagenation(..)
  , ListData(..)
-- for test
  , setRequestBodyJson
  , doRequest
  , parseRateLimit
  , parsePagenation
  , parsePagenationCore
  , onePagenationParser
  , tagsUrl
  ) where

import Control.Applicative ((<*>), (<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import Data.Functor ((<$>))
import Data.Maybe
import GHC.Exception (throw)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Types.Header

import qualified Text.Parsec as P
import Text.Parsec.String
import Data.Attoparsec.Char8 (char)

{- ------------------------------------------
 - data difinitions.
------------------------------------------- -}

data QiitaError = QiitaError { errorMessage :: String }
                  deriving (Show, Generic)
data Auth = Auth { token :: String }
            deriving (Show, Eq, Generic)
data RateLimit = RateLimit {remaining :: Int, limit :: Int }
                 deriving (Show, Eq, Generic)
data User = User { name :: String
                 , url_name :: String
                 , profile_image_url :: String
                 , url :: String
                 , description :: String
                 , website_url :: String
                 , organization :: String
                 , location :: String
                 , facebook :: String
                 , linkedin :: String
                 , twitter :: String
                 , github :: String
                 , followers :: Int
                 , following_users :: Int
                 , items :: Int
                 } deriving (Show, Eq, Generic)
data QiitaContext = QiitaContext { auth :: Auth, rateLimit :: RateLimit }
                    deriving (Show, Eq, Generic)
data Pagenation = Pagenation { pageRel :: ByteString, pageUrl :: ByteString }
                  deriving (Show, Eq)
-- 
-- 未認証な状態で取得可能なタグ情報.
-- 
data Tag = Tag { tag_name :: String
                 , tag_url_name :: String
                 , tag_icon_url :: String
                 , tag_item_count :: Int
                 , tag_follower_count :: Int
               } deriving (Show, Eq)
-- 
-- 認証済みユーザが取得可能なタグ情報.
-- 
data TagA = TagA { taga_name :: String
                 , taga_url_name :: String
                 , taga_icon_url :: String
                 , taga_item_count :: Int
                 , taga_follower_count :: Int
                 , taga_following :: Bool
               } deriving (Show, Eq)
data ListData a = ListData { list :: [a], pagenation :: [Pagenation] }
                  deriving (Show, Eq)

{- ------------------------------------------
 - type alias.
------------------------------------------- -}
type UserName = ByteString
type Password = ByteString
type PerPage = Int

{- ------------------------------------------
 - instance difinitions.
------------------------------------------- -}

instance FromJSON QiitaError
instance FromJSON Auth
instance FromJSON RateLimit
instance FromJSON User

-- 一部セレクタの名前がかぶるので手動でパースを書かざるを得ない.
instance FromJSON Tag where
  parseJSON (Object v) = Tag <$>
                            v .: "name"
                            <*> v .: "url_name"
                            <*> v .: "icon_url" 
                            <*> v .: "item_count"
                            <*> v .: "follower_count"
  parseJSON _          = mzero

instance FromJSON TagA where
  parseJSON (Object v) = TagA <$>
                            v .: "name"
                            <*> v .: "url_name"
                            <*> v .: "icon_url" 
                            <*> v .: "item_count"
                            <*> v .: "follower_count"
                            <*> v .: "following"
  parseJSON _          = mzero

{- ------------------------------------------
 - constants.
------------------------------------------- -}
defaultPerPage = 20
endpoint = "https://qiita.com/api/v1"
authUrl = endpoint ++ "/auth"
rateLimitUrl = endpoint ++ "/rate_limit"
userUrl = endpoint ++ "/user"
usersUrl = endpoint ++ "/users"
tagsUrl = endpoint ++ "/tags"

{- ------------------------------------------
 - public functions.
------------------------------------------- -}

{- ------------------------------------------
 - Qiitaに認証を投げ、エラー処理あるいは主処理を実行します.
------------------------------------------- -}
withAuthentication :: UserName -> Password
                   -> (QiitaError -> RateLimit -> IO a) -- エラー処理
                   -> (QiitaContext -> IO a) -- 主処理
                   -> IO a
withAuthentication user pass errorHandler mainOperation = do
  -- リクエストの組み立て
  req <- parseUrl authUrl
           >>= return . urlEncodedBody [("url_name", user), ("password", pass)]
           >>= \request -> return (request { checkStatus = checkStatus' })
  -- 通信実行
  res <- withManager $ \manager -> httpLbs req manager
  -- レスポンスの処理
  let rateLimit = parseRateLimit res
  let eAuth = decodeJsonBody $ responseBody res
  case eAuth of
    Left  err  -> errorHandler err rateLimit
    Right auth -> mainOperation $ QiitaContext { auth = auth, rateLimit = rateLimit }

{- ------------------------------------------
 - 未ログインユーザのAPI実行回数を得る.
------------------------------------------- -}
getAnonymousRateLimit :: IO RateLimit
getAnonymousRateLimit = simpleHttp rateLimitUrl
  >>= return . fromJust . decode

{- ------------------------------------------
 - ログイン済みユーザのAPI実行回数を得る.
------------------------------------------- -}
getRateLimit :: Auth -> IO RateLimit
getRateLimit auth = simpleHttp (rateLimitUrl ++ (tok auth))
  >>= return . fromJust . decode


{- ------------------------------------------
 - 任意のユーザのAPI実行回数を得る.
------------------------------------------- -}
getUserInformation :: UserName -> IO User
getUserInformation user = do
  req <- parseUrl (usersUrl ++ "/" ++ (C8.unpack user))
  res <- doRequest req
  return $ fromJust $ decode $ responseBody res

{- ------------------------------------------
 - ログイン済みユーザの情報を得る.
 - StateTの1つ目の型引数QiitaContextはアプリケーションの内部状態.
------------------------------------------- -}
getLoginUserInformation :: StateT QiitaContext IO User
getLoginUserInformation = do
  ctx <- get -- StateTからQiitaContextを取り出す
  req <- parseUrl (userUrl ++ (tok $ auth $ ctx))
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res } -- StateTに新しいQiitaContextを格納する
  return $ fromJust $ decode $ responseBody res

{- ------------------------------------------
 - タグ一覧を得るための一連の関数.
------------------------------------------- -}

getTagsFirstPage' :: PerPage -> IO (ListData Tag, RateLimit)
getTagsFirstPage' perPage = do
  req <- parseUrl (tagsUrl ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = tags, pagenation = ps }, rateLimit)

getTagsFirstPage :: IO (ListData Tag, RateLimit)
getTagsFirstPage = getTagsFirstPage' defaultPerPage

getTagsAFirstPage' :: PerPage -> StateT QiitaContext IO (ListData TagA)
getTagsAFirstPage' perPage = do
  ctx <- get
  req <- parseUrl (tagsUrl ++ (tok $ auth $ ctx) ++ "&per_page=" ++ (show perPage))
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list = tags, pagenation = ps }

getTagsAFirstPage :: StateT QiitaContext IO (ListData TagA)
getTagsAFirstPage = getTagsAFirstPage' defaultPerPage

{- ------------------------------------------
 - private functions.
------------------------------------------- -}

tok auth = "?token=" ++ (token auth)

checkStatus' status headers
  | statusCode status < 500 = Nothing
  | otherwise               = throw $ StatusCodeException status headers

decodeJsonBody :: (FromJSON a) => L.ByteString -> Either QiitaError a
decodeJsonBody body = case decode body of
  Just auth -> Right auth
  Nothing   -> case (decode body :: Maybe QiitaError) of
                 Nothing -> Left $ QiitaError { errorMessage = "Unknown Error." }
                 Just e  -> Left e

parseRateLimit :: Response b -> RateLimit
parseRateLimit res = let headers = responseHeaders res in
  RateLimit {
    limit = lookupIntValue "X-RateLimit-Limit" headers
    , remaining = lookupIntValue "X-RateLimit-Remaining" headers
  }

-- doRequest
--   :: (MonadIO m, Control.Monad.Trans.Control.MonadBaseControl IO m,
--       Control.Monad.Trans.Resource.MonadUnsafeIO m,
--       Control.Monad.Trans.Resource.MonadThrow m) =>
--      Request (Control.Monad.Trans.Resource.ResourceT m)
--      -> m (Response LC.ByteString)
doRequest req = withManager (\manager -> httpLbs req manager)

lookupIntValue :: HeaderName -> [Header] -> Int
lookupIntValue headerName headers = case lookup headerName headers of
                                          Nothing  -> 999
                                          Just val -> read $ C8.unpack val

setRequestBodyJson :: (ToJSON j) => j -> Request m -> Request m
setRequestBodyJson entity req = req {
                                  requestBody = RequestBodyLBS $ encode entity
                                  , method = "POST"
                                  , requestHeaders = [("content-type","application/json")]
                                }

parsePagenation :: Response b -> [Pagenation]
parsePagenation res = case lookup "Link" $ responseHeaders res of
                        Nothing -> []
                        Just l  -> parsePagenationCore l

{- ------------------------------------------
- パース対象のヘッダは以下のようなもの.
- Link: <https://qiita.com/api/v1/tags.json?page=3&per_page=30>; rel="next", <https://qiita.com/api/v1/tags.json?page=45&per_page=30>; rel="last"
------------------------------------------- -}
parsePagenationCore :: ByteString -> [Pagenation]
parsePagenationCore source = case P.parse pagenationParser "" (C8.unpack source) of
                               Left  _  -> []
                               Right ps -> ps

pagenationParser :: Parser [Pagenation]
pagenationParser = P.many1 onePagenationParser

onePagenationParser :: Parser Pagenation
onePagenationParser = do
  P.spaces
  P.char '<'
  url <- P.many1 $ P.noneOf ">"
  P.string ">; rel=\""
  rel <- P.many1 $ P.noneOf "\""
  P.try (P.string "\"," >> P.spaces) <|> P.spaces
  return $ Pagenation { pageUrl = C8.pack url, pageRel = C8.pack rel }

