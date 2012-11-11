{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.QiitaTest where

import Jabara.Qiita

import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Conduit

{-
run = do
  ma <- authenticate "jabaraster" "xxxxxxxx"
  let auth = case ma of
               Left  m    -> Prelude.error $ show m
               Right auth -> auth
  getLoginUserInformation auth >>= print

run2 = do
  ma <- authenticate "jabaraster" "xxxxxxxx"
  let auth = fromRight ma
  simpleHttp ("https://qiita.com/api/v1/user?token=" ++ (token auth))
-}

run = withAuthenticate "jabaraster" "w9tau9Em"
        (\err limit -> print err >> print limit)
        (\ctx -> evalStateT core ctx)

core = do
  ctx1 <- get
  liftIO $ putStrLn ("Pre: " ++ (show ctx1))

  user <- getLoginUserInformation'
  liftIO $ print user
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  user' <- getLoginUserInformation'
  liftIO $ print user'
  ctx3 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx3))

fromRight :: (Show l) => Either l r -> r
fromRight (Left left) = Prelude.error $ show left
fromRight (Right right) = right

instance ToJSON User

j = LC.pack "{\"name\":\"jabaraster\",\"url_name\":\"jabaraster\",\"profile_image_url\":\"https://si0.twimg.com/profile_images/1272321155/duke_globe_normal.gif\",\"url\":\"http://qiita.com/users/jabaraster\",\"description\":\"\",\"website_url\":\"\",\"organization\":\"\",\"location\":\"Kumamoto, Japan\",\"facebook\":\"\",\"linkedin\":\"\",\"twitter\":\"jabaraster\",\"github\":\"jabaraster\",\"followers\":2,\"following_users\":4,\"items\":8}"

