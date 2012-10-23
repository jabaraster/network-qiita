{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.Qiita (
  authenticate
  , getAnonymousRateLimit
  , getRateLimit
  , getLoginUserInformation
  , QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  ) where

import Control.Applicative ((<*>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, FromJSON, parseJSON, (.:), decode)
import Data.Functor ((<$>))
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import GHC.Exception (throw)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import Network.HTTP.Types

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


{- ------------------------------------------
 - Qiitaに認証を投げトークンを得る
------------------------------------------- -}
authenticate :: ByteString -> ByteString -> IO (Either QiitaError Auth)
authenticate user pass = liftIO $ do
  parseUrl authUrl
  >>= return . urlEncodedBody [("url_name", user), ("password", pass)]
  >>= \request -> return (request { checkStatus = checkStatus' })
  >>= requestJson

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
------------------------------------------- -}
getLoginUserInformation :: Auth -> IO User
getLoginUserInformation auth = simpleHttp (userUrl ++ (tok auth))
  >>= return . fromJust . decode
{- ------------------------------------------
 - private functions.
------------------------------------------- -}

tok auth = "?token=" ++ (token auth)

checkStatus' status headers
  | statusCode status < 500 = Nothing
  | otherwise               = throw $ StatusCodeException status headers

--decodeJsonResponse :: (FromJSON a, Control.Monad.IO.Class.MonadIO m,
--      Control.Monad.Trans.Control.MonadBaseControl IO m, MonadUnsafeIO m,
--      MonadThrow m) =>
--     Request (ResourceT m) -> m (Either QiitaError a)
requestJson req = do
  withManager $ \manager -> do
    res <- httpLbs req manager
    return $ decodeJsonBody $ responseBody res

decodeJsonBody :: (FromJSON a) => L.ByteString -> Either QiitaError a
decodeJsonBody body = case decode body of
  Just auth -> Right auth
  Nothing   -> case (decode body :: Maybe QiitaError) of
                 Nothing -> Left $ QiitaError { errorMessage = "Unknown Error." }
                 Just e  -> Left e
