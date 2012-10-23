{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Jabara.QiitaTest (run) where

import Jabara.Qiita

run = do
  ma <- authenticate "jabaraster" ""
  let auth = case ma of
               Left  m    -> Prelude.error $ show m
               Right auth -> auth
  getRateLimit auth >>= print
