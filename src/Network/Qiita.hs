{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Qiita (
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
  , getItemsFirstPage'
  , getItemsFirstPage
  , getItemsWithPage
  , getItemsAFirstPage'
  , getItemsAFirstPage
  , getItemsAWithPage
  , getStocksAFirstPage'
  , getStocksAFirstPage
  , getStocksAWithPage
  , getUserFollowingTagsFirstPage'
  , getUserFollowingTagsFirstPage
  , getUserFollowingTagsWithPage
  , getUserFollowingTagsAFirstPage'
  , getUserFollowingTagsAFirstPage
  , getUserFollowingTagsAWithPage
  , getUserFollowingUsersFirstPage'
  , getUserFollowingUsersFirstPage
  , getUserFollowingUsersWithPage
  , getUserFollowingUsersAFirstPage'
  , getUserFollowingUsersAFirstPage
  , getUserFollowingUsersAWithPage
  , getUserStocksFirstPage'
  , getUserStocksFirstPage
  , getUserStocksWithPage
  , getUserStocksAFirstPage'
  , getUserStocksAFirstPage
  , getUserStocksAWithPage
  , getUserItemsFirstPage'
  , getUserItemsFirstPage
  , getUserItemsWithPage
  , getUserItemsAFirstPage'
  , getUserItemsAFirstPage
  , getUserItemsAWithPage
  , searchItemsFirstPage'
  , searchItemsFirstPage
  , searchStockedItemsAFirstPage'
  , searchStockedItemsAFirstPage
  , searchItemsWithPage
  , searchItemsAFirstPage'
  , searchItemsAFirstPage
  , searchItemsAWithPage
  , getTagItemsFirstPage'
  , getTagItemsFirstPage
  , getTagItemsWithPage
  , postItem
  , updateItem
  , deleteItem
  , getItem
  , stockItem
  , unstockItem
  , QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  , Tag(..)
  , TagA(..)
  , PostItem(..)
  , UpdateItem(..)
  , PostTag(..)
  , QiitaContext(..)
  , Pagenation(..)
  , ListData(..)
  , ItemUser(..)
  , ItemTag(..)
  , Item(..)
  , FullItem(..)
  , UserName
  , Password
  , PerPage
  , ItemUuid
  , Q
  , itemToUpdateItem
  ) where

import Network.Qiita.Types

import Control.Monad.State
import Data.Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8

import Data.Maybe
import GHC.Exception (throw)

import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.URI

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
stocksUrl = endpoint ++ "/stocks"
searchUrl = endpoint ++ "/search"

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
 - 新着投稿を得るための一連の関数.
------------------------------------------- -}

getItemsFirstPage' :: PerPage -> IO (ListData Item)
getItemsFirstPage' perPage = do
  req <- parseUrl (itemsUrl ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps })

getItemsFirstPage :: IO (ListData Item)
getItemsFirstPage = getItemsFirstPage' defaultPerPage

getItemsWithPage :: Pagenation -> IO (ListData Item)
getItemsWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list = items, pagenation = ps }

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

getItemsAWithPage :: Pagenation -> StateT QiitaContext IO (ListData Item)
getItemsAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  ctx <- get
  put $ ctx { rateLimit = parseRateLimit res }
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list = items, pagenation = ps }

{- ------------------------------------------
 - 自分のストックした投稿を得るための一連の関数.
------------------------------------------- -}

getStocksAFirstPage' :: PerPage -> StateT QiitaContext IO (ListData Item)
getStocksAFirstPage' perPage = do
  ctx <- get
  req <- parseUrl (stocksUrl ++ (tok $ auth $ ctx) ++ "&per_page=" ++ (show perPage))
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list =items, pagenation = ps }

getStocksAFirstPage :: StateT QiitaContext IO (ListData Item)
getStocksAFirstPage = getStocksAFirstPage' defaultPerPage

getStocksAWithPage :: Pagenation -> StateT QiitaContext IO (ListData Item)
getStocksAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  ctx <- get
  put $ ctx { rateLimit = parseRateLimit res }
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ ListData { list = items, pagenation = ps }

{- -----------------------------------------------------------
 - 特定のユーザーがフォローしているタグを得るための一連の関数.
------------------------------------------------------------ -}
getUserFollowingTagsAFirstPage' :: UserName -> PerPage -> IO (ListData FollowingTag, RateLimit)
getUserFollowingTagsAFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/following_tags" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = tags, pagenation = ps }, rateLimit)

getUserFollowingTagsAFirstPage :: UserName -> IO (ListData FollowingTag, RateLimit)
getUserFollowingTagsAFirstPage =  (flip getUserFollowingTagsAFirstPage') defaultPerPage

getUserFollowingTagsAWithPage :: Pagenation -> IO (ListData FollowingTag, RateLimit)
getUserFollowingTagsAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = tags, pagenation = ps }, rateLimit)

getUserFollowingTagsFirstPage' :: UserName -> PerPage -> IO (ListData FollowingTag, RateLimit)
getUserFollowingTagsFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/following_tags" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = tags, pagenation = ps }, rateLimit)

getUserFollowingTagsFirstPage :: UserName -> IO (ListData FollowingTag, RateLimit)
getUserFollowingTagsFirstPage =  (flip getUserFollowingTagsFirstPage') defaultPerPage

getUserFollowingTagsWithPage :: Pagenation -> IO (ListData FollowingTag, RateLimit)
getUserFollowingTagsWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let tags = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = tags, pagenation = ps }, rateLimit)

{- ---------------------------------------------------------------
 - 特定のユーザーがフォローしているユーザーを得るための一連の関数.
---------------------------------------------------------------- -}
getUserFollowingUsersAFirstPage' :: UserName -> PerPage -> IO (ListData FollowingUser, RateLimit)
getUserFollowingUsersAFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/following_users" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let users = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = users, pagenation = ps }, rateLimit)

getUserFollowingUsersAFirstPage :: UserName -> IO (ListData FollowingUser, RateLimit)
getUserFollowingUsersAFirstPage =  (flip getUserFollowingUsersAFirstPage') defaultPerPage

getUserFollowingUsersAWithPage :: Pagenation -> IO (ListData FollowingUser, RateLimit)
getUserFollowingUsersAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let users = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = users, pagenation = ps }, rateLimit)

getUserFollowingUsersFirstPage' :: UserName -> PerPage -> IO (ListData FollowingUser, RateLimit)
getUserFollowingUsersFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/following_users" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let users = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = users, pagenation = ps }, rateLimit)

getUserFollowingUsersFirstPage :: UserName -> IO (ListData FollowingUser, RateLimit)
getUserFollowingUsersFirstPage =  (flip getUserFollowingUsersFirstPage') defaultPerPage

getUserFollowingUsersWithPage :: Pagenation -> IO (ListData FollowingUser, RateLimit)
getUserFollowingUsersWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let users = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = users, pagenation = ps }, rateLimit)

{- -----------------------------------------------------
 - 特定ユーザーのストックした投稿を得るための一連の関数.
----------------------------------------------------- -}
getUserStocksAFirstPage' :: UserName -> PerPage -> IO (ListData Item, RateLimit)
getUserStocksAFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/stocks" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getUserStocksAFirstPage :: UserName -> IO (ListData Item, RateLimit)
getUserStocksAFirstPage =  (flip getUserStocksAFirstPage') defaultPerPage

getUserStocksAWithPage :: Pagenation -> IO (ListData Item, RateLimit)
getUserStocksAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getUserStocksFirstPage' :: UserName -> PerPage -> IO (ListData Item, RateLimit)
getUserStocksFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/stocks" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getUserStocksFirstPage :: UserName -> IO (ListData Item, RateLimit)
getUserStocksFirstPage =  (flip getUserStocksFirstPage') defaultPerPage

getUserStocksWithPage :: Pagenation -> IO (ListData Item, RateLimit)
getUserStocksWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let users = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = users, pagenation = ps }, rateLimit)

{- -----------------------------------------
 - 特定ユーザーの投稿を得るための一連の関数.
----------------------------------------- -}
getUserItemsAFirstPage' :: UserName -> PerPage -> IO (ListData Item, RateLimit)
getUserItemsAFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/items" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getUserItemsAFirstPage :: UserName -> IO (ListData Item, RateLimit)
getUserItemsAFirstPage =  (flip getUserItemsAFirstPage') defaultPerPage

getUserItemsAWithPage :: Pagenation -> IO (ListData Item, RateLimit)
getUserItemsAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getUserItemsFirstPage' :: UserName -> PerPage -> IO (ListData Item, RateLimit)
getUserItemsFirstPage' userName perPage = do
  req <- parseUrl (usersUrl ++ "/" ++ C8.unpack userName ++ "/items" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getUserItemsFirstPage :: UserName -> IO (ListData Item, RateLimit)
getUserItemsFirstPage =  (flip getUserItemsFirstPage') defaultPerPage

getUserItemsWithPage :: Pagenation -> IO (ListData Item, RateLimit)
getUserItemsWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let users = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = users, pagenation = ps }, rateLimit)

{- -----------------------------------------
 - 検索結果を得るための一連の関数.
----------------------------------------- -}
searchItemsAFirstPage' :: Q -> PerPage -> IO (ListData Item, RateLimit)
searchItemsAFirstPage' q perPage = do
  req <- parseUrl (searchUrl ++ "?q=" ++ (escapeURIString isAllowedInURI $ C8.unpack q) ++ "&per_page=" ++ (show perPage))
  res <- doRequest req
  -- レスポンスの処理
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

searchItemsAFirstPage :: Q -> IO (ListData Item, RateLimit)
searchItemsAFirstPage =  (flip searchItemsAFirstPage') defaultPerPage

searchStockedItemsAFirstPage' :: Q -> PerPage -> StateT QiitaContext IO (ListData Item)
searchStockedItemsAFirstPage' q perPage = do
  ctx <- get
  req <- parseUrl (searchUrl ++ (tok $ auth $ ctx) ++ "&q=" ++ (escapeURIString isAllowedInURI $ C8.unpack q) ++ "&stocked=True&per_page=" ++ (show perPage))
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  -- レスポンスの処理
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps })

searchStockedItemsAFirstPage :: Q -> StateT QiitaContext IO (ListData Item)
searchStockedItemsAFirstPage =  (flip searchStockedItemsAFirstPage') defaultPerPage

searchItemsAWithPage :: Pagenation -> IO (ListData Item, RateLimit)
searchItemsAWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

searchItemsFirstPage' :: Q -> PerPage -> IO (ListData Item, RateLimit)
searchItemsFirstPage' q perPage = do
  req <- parseUrl (searchUrl ++ "?q=" ++ (escapeURIString isAllowedInURI $ C8.unpack q) ++ "&per_page=" ++ (show perPage))
  res <- doRequest req
  -- レスポンスの処理
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

searchItemsFirstPage :: Q -> IO (ListData Item, RateLimit)
searchItemsFirstPage =  (flip searchItemsFirstPage') defaultPerPage

searchItemsWithPage :: Pagenation -> IO (ListData Item, RateLimit)
searchItemsWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let users = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = users, pagenation = ps }, rateLimit)

{- ------------------------------------------
 - 特定タグの投稿を得るための一連の関数.
------------------------------------------- -}
getTagItemsFirstPage' :: TagName -> PerPage -> IO (ListData Item, RateLimit)
getTagItemsFirstPage' tagName perPage = do
  req <- parseUrl (tagsUrl ++ "/" ++ C8.unpack tagName ++ "/items" ++ "?per_page=" ++ (show perPage))
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

getTagItemsFirstPage :: TagName -> IO (ListData Item, RateLimit)
getTagItemsFirstPage =  (flip getTagItemsFirstPage') defaultPerPage

getTagItemsWithPage :: Pagenation -> IO (ListData Item, RateLimit)
getTagItemsWithPage pagenation = do
  req <- parseUrl $ C8.unpack $ pageUrl pagenation
  res <- doRequest req
  let rateLimit = parseRateLimit res
  let items = fromJust $ decode $ responseBody res
  let ps = parsePagenation res
  return $ (ListData { list = items, pagenation = ps }, rateLimit)

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
 - 投稿の更新
------------------------------------------- -}
updateItem :: UpdateItem -> StateT QiitaContext IO (Either QiitaError Item)
updateItem item = do
  ctx <- get
  req <- parseUrl (buildItemUrl' (update_item_uuid item)  ctx)
           >>= return . setRequestBodyJson' methodPut item
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  return $ decodeJsonBody $ responseBody res

{- ------------------------------------------
 - 投稿の削除
------------------------------------------- -}
deleteItem :: ItemUuid -> StateT QiitaContext IO (Either QiitaError ())
deleteItem uuid = do
  ctx <- get
  req <- parseUrl (buildItemUrl uuid ctx)
           >>= \r -> return $ r { method = methodDelete }
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  return $ Right ()

{- ------------------------------------------
 - 特定の投稿取得
------------------------------------------- -}
getItem :: ItemUuid -> IO (Either RateLimit (FullItem, RateLimit))
getItem uuid = do
  req <- parseUrl (itemsUrl ++ "/" ++ C8.unpack uuid)
           >>= \r -> return $ r { checkStatus = checkStatus' }
  res <- doRequest req

  let rateLimit = parseRateLimit res

  case decode $ responseBody res of
    Nothing -> return $ Left rateLimit
    Just item -> return $ Right (item, rateLimit)

{- ------------------------------------------
 - 投稿のストック
------------------------------------------- -}
stockItem :: ItemUuid -> StateT QiitaContext IO (Either QiitaError ())
stockItem uuid = do
  ctx <- get
  req <- parseUrl (buildStockUrl uuid ctx)
           >>= \r -> return $ r { method = methodPut }
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  return $ Right ()

{- ------------------------------------------
 - 投稿のストック解除
------------------------------------------- -}
unstockItem :: ItemUuid -> StateT QiitaContext IO (Either QiitaError ())
unstockItem uuid = do
  ctx <- get
  req <- parseUrl (buildStockUrl uuid ctx)
           >>= \r -> return $ r { method = methodDelete }
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res }
  return $ Right ()

{- ------------------------------------------
 - private functions.
------------------------------------------- -}

tok auth = "?token=" ++ (token auth)

checkStatus' status headers
  | statusCode status < 500 = Nothing
  | otherwise                  = throw $ StatusCodeException status headers

decodeJsonBody :: (FromJSON a) => LBS.ByteString -> Either QiitaError a
decodeJsonBody body = case decode body of
  Just ret -> Right ret
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
                                  , method = methodPost
                                  , requestHeaders = [ ("content-type","application/json")
                                                     , ("user-agent","Jabara.Qiita/1.0")
                                                     ]
                                }

setRequestBodyJson' :: (ToJSON j) => Method -> j -> Request m -> Request m
setRequestBodyJson' method entity req = req {
                                          requestBody = RequestBodyLBS $ encode entity
                                          , method = method
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

buildStockUrl :: ItemUuid -> QiitaContext -> String
buildStockUrl uuid ctx = itemsUrl ++ "/" ++ (C8.unpack uuid) ++ "/stock" ++ (tok $ auth $ ctx)

buildItemUrl :: ItemUuid -> QiitaContext -> String
buildItemUrl uuid ctx = itemsUrl ++ "/" ++ (C8.unpack uuid) ++ (tok $ auth $ ctx)

buildItemUrl' :: String -> QiitaContext -> String
buildItemUrl' uuid ctx = itemsUrl ++ "/" ++ uuid ++ (tok $ auth $ ctx)
