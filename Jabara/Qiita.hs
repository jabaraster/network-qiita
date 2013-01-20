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
  , getTagsWithPage
  , getTagsAFirstPage
  , getTagsAFirstPage'
  , getTagsAWithPage
  , postItem
  , QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  , Tag(..)
  , PostItem(..)
  , PostTag(..)
  , QiitaContext(..)
  , Pagenation(..)
  , ListData(..)
  , ItemUser(..)
  , ItemTag(..)
  , Item(..)
-- for test
--  , setRequestBodyJson
--  , doRequest
--  , parseRateLimit
--  , parsePagenation
--  , parsePagenationCore
--  , onePagenationParser
  ) where

import Control.Applicative ((<*>), (<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.Vector as V
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

data PostItem = PostItem { title :: String
                         , body :: String
                         , tags :: [PostTag]
                         , private :: Bool
                         , gist :: Bool
                         , tweet :: Bool
                         } deriving (Show, Eq)

data PostTag = PostTag { post_tag_name :: String
                       , post_tag_versions :: [String]
                       } deriving (Show, Eq)

data ItemUser = ItemUser { item_user_name :: String
                         , item_user_url_name :: String
                         , item_user_profile_image_url :: String
                         } deriving (Show, Eq)
data ItemTag = ItemTag { item_tag_name :: String
                       , item_tag_url_name :: String
                       , item_tag_icon_url :: String
                       , item_tag_versions :: [String]
                       } deriving (Show, Eq)
data Item = Item { item_id :: Integer
                 , item_uuid :: String
                 , item_user :: ItemUser
                 , item_title :: String
                 , item_body :: String
                 , item_created_at :: String -- TODO 相応しいデータ型
                 , item_updated_at :: String -- TODO
                 , item_created_at_in_words :: String
                 , item_updated_at_in_words :: String
                 , item_tags :: [ItemTag]
                 , item_stock_count :: Integer
                 , item_stock_users :: [String]
                 , item_comment_count :: Integer
                 , item_url :: String
                 , item_gist_url :: Maybe String
                 , item_tweet :: Bool
                 , item_private :: Bool
--                 , item_stocked :: Bool -- これはレスポンスのJSONに入ってこない
                 } deriving (Show, Eq)

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

instance ToJSON PostItem where
  toJSON pi = object [
                "title" .= title pi
                , "body" .= body pi
                , "tags" .= buildTags
                , "private" .= private pi
                , "gist" .= gist pi
                , "tweet" .= tweet pi
              ]
    where
      buildTags = V.fromList $ Prelude.map tagToObject $ tags pi
      tagToObject tag = object [ "name" .= post_tag_name tag
                               , "versions" .= (V.fromList $ post_tag_versions tag)
                               ]
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
 - ItemをJSONに変換するロジック.
 - プロパティが多くなってきたときにこの表記法はかなりイヤなのだが、
 - 他に良い書き方を調査する時間がない・・・
------------------------------------------- -}
instance FromJSON Item where
  parseJSON (Object v) = Item <$>
                         v .: "id"
                         <*> v .: "uuid"
                         <*> v .: "user"
                         <*> v .: "title"
                         <*> v .: "body"
                         <*> v .: "created_at"
                         <*> v .: "updated_at"
                         <*> v .: "created_at_in_words"
                         <*> v .: "updated_at_in_words"
                         <*> v .: "tags"
                         <*> v .: "stock_count"
                         <*> v .: "stock_users"
                         <*> v .: "comment_count"
                         <*> v .: "url"
                         <*> v .: "gist_url"
                         <*> v .: "tweet"
                         <*> v .: "private"
  parseJSON _          = mzero
instance FromJSON ItemUser where
  parseJSON (Object v) = ItemUser <$>
                         v .: "name"
                         <*> v .: "url_name"
                         <*> v .: "profile_image_url"
  parseJSON _          = mzero
instance FromJSON ItemTag where
  parseJSON (Object v) = ItemTag <$>
                         v .: "name"
                         <*> v .: "url_name"
                         <*> v .: "icon_url"
                         <*> v .: "versions"
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
itemsUrl = endpoint ++ "/items"

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

getTagsWithPage :: Pagenation -> IO (ListData Tag, RateLimit)
getTagsWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  let rateLimit = parseRateLimit res
  let list = ListData { list = tags, pagenation = ps }
  return $ (list, rateLimit)

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

getTagsAWithPage :: Pagenation -> StateT QiitaContext IO (ListData TagA)
getTagsAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  ctx <- get
  put $ ctx { rateLimit = parseRateLimit res }
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list = tags, pagenation = ps }

{- ------------------------------------------
 - 投稿の実行
 - TODO エラーは応答コードとX-Response-Body-Startヘッダを解釈する必要がありそう.
------------------------------------------- -}
postItem :: PostItem -> StateT QiitaContext IO (Either QiitaError Item)
postItem item = do
  ctx <- get
  req <- parseUrl (itemsUrl  ++ (tok $ auth $ ctx))
           >>= return . setRequestBodyJson item
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  return $ decodeJsonBody $ responseBody res

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
                                  , requestHeaders = [ ("content-type","application/json")
                                                     , ("user-agent","Jabara.Qiita/1.0")
                                                     ]
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

