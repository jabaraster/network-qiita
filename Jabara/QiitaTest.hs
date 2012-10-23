{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.QiitaTest where

import Jabara.Qiita
import Data.Aeson

import Network.HTTP.Conduit

run = do
  ma <- authenticate "jabaraster" "w9tau9Em"
  let auth = case ma of
               Left  m    -> Prelude.error $ show m
               Right auth -> auth
  getLoginUserInformation auth >>= print

run2 = do
  ma <- authenticate "jabaraster" "w9tau9Em"
  let auth = fromRight ma
  simpleHttp ("https://qiita.com/api/v1/user?token=" ++ (token auth))

fromRight :: (Show l) => Either l r -> r
fromRight (Left left) = Prelude.error $ show left
fromRight (Right right) = right

instance ToJSON User
