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
  , getItemsFirstPage
  , getItemsAFirstPage
  , getItemsAFirstPagePerPage
  , getItemsAWithPage
  , getTagItemsFirstPage'
  , getTagItemsFirstPage
  , postItem
  , QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  , Tag(..)
  , TagA(..)
  , PostItem(..)
  , PostTag(..)
  , QiitaContext(..)
  , Pagenation(..)
  , ListData(..)
  , ItemUser(..)
  , ItemTag(..)
  , Item(..)
  , UserName
  , Password
  , PerPage
-- for test
--  , setRequestBodyJson
--  , doRequest
--  , parseRateLimit
--  , parsePagenation
--  , parsePagenationCore
--  , onePagenationParser
  ) where

import Jabara.Qiita.Types

import Control.Monad.State
import Data.Aeson
import Data.Attoparsec.Char8 (char)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8

import Data.Maybe
import GHC.Exception (throw)

import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Types.Header

import qualified Text.Parsec as P
import Text.Parsec.String

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

-- mats' addition starts here
{- ------------------------------------------
 - 新着投稿を得るための一連の関数.
------------------------------------------- -}

getItemsFirstPage' :: PerPage -> IO (ListData Item, RateLimit)
getItemsFirstPage' perPage = do
  req <- parseUrl (itemsUrl ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getItemsFirstPage :: IO (ListData Item, RateLimit)
getItemsFirstPage = getItemsFirstPage' defaultPerPage

getItemsAFirstPage' :: PerPage -> StateT QiitaContext IO (ListData Item)
getItemsAFirstPage' perPage = do
  ctx <- get
  req <- parseUrl (itemsUrl ++ (tok $ auth $ ctx) ++ "&per_page=" ++ (show perPage))
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list =items, pagenation = ps }

getItemsAFirstPage :: StateT QiitaContext IO (ListData Item)
getItemsAFirstPage = getItemsAFirstPage' defaultPerPage

getItemsAFirstPagePerPage :: PerPage -> StateT QiitaContext IO (ListData Item)
getItemsAFirstPagePerPage perPage = getItemsAFirstPage' perPage

getItemsAWithPage :: Pagenation -> StateT QiitaContext IO (ListData Item)
getItemsAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  ctx <- get
  put $ ctx { rateLimit = parseRateLimit res }
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list = items, pagenation = ps }
-- mats' addition ends here

getTagItemsFirstPage' :: TagName -> PerPage -> IO (ListData Item, RateLimit)
getTagItemsFirstPage' tagName perPage = do
  req <- parseUrl (tagsUrl ++ "/" ++ C8.unpack tagName ++ "/items" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getTagItemsFirstPage :: TagName -> IO (ListData Item, RateLimit)
getTagItemsFirstPage =  undefined
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

decodeJsonBody :: (FromJSON a) => LBS.ByteString -> Either QiitaError a
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
parsePagenationCore :: BS.ByteString -> [Pagenation]
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
  P.try (P.string "\"," >> P.spaces) P.<|> P.spaces
  return $ Pagenation { pageUrl = C8.pack url, pageRel = C8.pack rel }

