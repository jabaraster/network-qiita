{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.Qiita (
  authenticate
  , withAuthenticate
  , getAnonymousRateLimit
  , getRateLimit
  , getLoginUserInformation
  , QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  , QiitaContext(..)
-- for test
  ) where

import Control.Applicative ((<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Aeson (Object, FromJSON, parseJSON, (.:), decode)
import Data.Functor ((<$>))
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import Data.Maybe
import GHC.Exception (throw)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Types.Header

{- ------------------------------------------
 - data difinitions.
------------------------------------------- -}

data QiitaError = QiitaError { errorMessage :: String }
                  deriving (Show, Generic)
data Auth = Auth { token :: String }
            deriving (Show, Eq, Generic)
data RateLimit = RateLimit { remaining :: Int, limit :: Int }
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

{- ------------------------------------------
 - instance difinitions.
------------------------------------------- -}

instance FromJSON QiitaError
instance FromJSON Auth
instance FromJSON RateLimit
instance FromJSON User

{- ------------------------------------------
 - constants.
------------------------------------------- -}
endpoint = "https://qiita.com/api/v1"
authUrl = endpoint ++ "/auth"
rateLimitUrl = endpoint ++ "/rate_limit"
userUrl = endpoint ++ "/user"

{- ------------------------------------------
 - public functions.
------------------------------------------- -}

withAuthenticate :: ByteString -> ByteString
                   -> (QiitaError -> RateLimit -> IO a)
                   -> (QiitaContext -> IO a)
                   -> IO a
withAuthenticate user pass errorHandler mainOperation = do
  -- リクエストの組み立て
  req <- parseUrl authUrl
           >>= return . urlEncodedBody [("url_name", user), ("password", pass)]
           >>= \request -> return (request { checkStatus = checkStatus' })
  -- 通信実行
  res <- withManager $ \manager -> httpLbs req manager
  -- レスポンスの処理
  let rateLimit = parseRateLimit res
--  let headers = responseHeaders res
--  let limit = lookupIntValue "X-RateLimit-Limit" headers
--  let remaining = lookupIntValue "X-RateLimit-Remaining" headers
--  let rateLimit = RateLimit { remaining = remaining, limit = limit }
  let eAuth = decodeJsonBody $ responseBody res
  case eAuth of
    Left  err  -> errorHandler err rateLimit
    Right auth -> mainOperation $ QiitaContext { auth = auth, rateLimit = rateLimit }

{- ------------------------------------------
 - Qiitaに認証を投げトークンを得る
------------------------------------------- -}
-- authenticate :: ByteString -> ByteString -> IO (Either QiitaError Auth)
authenticate user pass = do
  req <- parseUrl authUrl
           >>= return . urlEncodedBody [("url_name", user), ("password", pass)]
           >>= \request -> return (request { checkStatus = checkStatus' })
  res <- withManager $ \manager -> httpLbs req manager
  let headers = responseHeaders res
  let limit = lookupIntValue "X-RateLimit-Limit" headers
  let remaining = lookupIntValue "X-RateLimit-Remaining" headers
  return $ RateLimit { remaining = remaining, limit = limit }

{- ------------------------------------------
 - 未ログインユーザのAPI実行回数を得る.
------------------------------------------- -}
getAnonymousRateLimit :: IO (RateLimit)
getAnonymousRateLimit = do
  r <- simpleHttp rateLimitUrl
  return $ fromJust $ decode r

{- ------------------------------------------
 - ログイン済みユーザのAPI実行回数を得る.
------------------------------------------- -}
getRateLimit :: Auth -> IO (RateLimit)
getRateLimit auth = simpleHttp (rateLimitUrl ++ (tok auth))
  >>= return . fromJust . decode

{- ------------------------------------------
 - ログイン済みユーザの情報を得る.
 - StateTの1つ目の型引数はアプリケーションの内部状態.
------------------------------------------- -}
getLoginUserInformation :: StateT QiitaContext IO User
getLoginUserInformation = do
  ctx <- get -- StateTからQiitaContextを取り出す
  req <- parseUrl (userUrl ++ (tok $ auth $ ctx))
  res <- doRequest req
  put $ ctx { rateLimit = parseRateLimit res} -- StateTに新しいQiitaContextを格納する
  return $ fromJust $ decode $ responseBody res

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

doRequest req = withManager (\manager -> httpLbs req manager)

lookupIntValue :: HeaderName -> [Header] -> Int
lookupIntValue headerName headers = case lookup headerName headers of
                                          Nothing  -> 999
                                          Just val -> read $ C8.unpack val

