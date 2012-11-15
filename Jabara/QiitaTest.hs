{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.QiitaTest where

import Jabara.Qiita

import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Generics (Generic)
import Network.HTTP.Conduit

{- ------------------------------------------
 - withAuthentication利用例.
 - 実行したら
 - 1. ユーザ名を入力してEnter
 - 2. パスワードを入力してEnter
 - とすると、先の処理が実行されます.
------------------------------------------- -}
run :: IO ()
run = do
  putStrLn "ユーザ名を入力してEnter、次にパスワードを入力してEnterを押すこと！"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
        (\ctx -> evalStateT runCore ctx) -- 認証OK後の処理

{- ------------------------------------------
 - Qiitaにアクセスする、主処理.
 - QiitaContextにいつでもアクセス出来るように、StateTという型を使う.
 - StateT QiitaContext m n という型に対して
 - getを呼ぶとQiitaContextを取得することが出来る.
------------------------------------------- -}
runCore :: StateT QiitaContext IO ()
runCore = do
  liftIO $ putStrLn "- 1. -----------------------------"
  ctx1 <- get
  liftIO $ putStrLn ("Pre: " ++ (show ctx1))

  liftIO $ putStrLn "- 2. -----------------------------"
  user <- getLoginUserInformation
  liftIO $ print user
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn "- 3. -----------------------------"
  user' <- liftIO $ getUserInformation "gishi_yama"
  liftIO $ print user'
  ctx3 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx3))





data UserModel = UserModel {
                   userName::B.ByteString
                   , userNickname::B.ByteString
                 } deriving (Show, Eq, Generic)

-- instance ToJSON User
instance ToJSON UserModel
instance FromJSON UserModel

j = LC.pack "{\"name\":\"jabaraster\",\"url_name\":\"jabaraster\",\"profile_image_url\":\"https://si0.twimg.com/profile_images/1272321155/duke_globe_normal.gif\",\"url\":\"http://qiita.com/users/jabaraster\",\"description\":\"\",\"website_url\":\"\",\"organization\":\"\",\"location\":\"Kumamoto, Japan\",\"facebook\":\"\",\"linkedin\":\"\",\"twitter\":\"jabaraster\",\"github\":\"jabaraster\",\"followers\":2,\"following_users\":4,\"items\":8}"


run2 :: IO LC.ByteString
run2 = do
  req <- parseUrl "https://qiita.com/api/v1/tags.json?page=3&per_page=30"
           >>= return . urlEncodedBody [("url", "")]
  liftIO $ print $ responseTimeout req
  res <- withManager $ \manager -> httpLbs req manager
  return $ responseBody res

run3 :: IO ()
run3 = do
  req <- parseUrl "http://localhost:8081/rest/test/"
           >>= return . setRequestBodyJson (UserModel {userName = "jabara", userNickname = "jabaraster"})
  res <- doRequest req
  print $ lookup "Link" $ responseHeaders res
  return ()

