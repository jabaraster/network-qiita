{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.QiitaTest where

import Jabara.Qiita

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe
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
        (\ctx -> evalStateT runCore2 ctx) -- 認証OK後の処理

{- ------------------------------------------
 - Qiitaにアクセスする、主処理.
 - QiitaContextにいつでもアクセス出来るように、StateTという型を使う.
 - StateT QiitaContext m n という型に対して
 - getを呼ぶとQiitaContextを取得することが出来る.
------------------------------------------- -}
runCore :: StateT QiitaContext IO ()
runCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  ctx1 <- get
  liftIO $ putStrLn ("Pre: " ++ (show ctx1))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  user <- getLoginUserInformation
  liftIO $ print user
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  user' <- liftIO $ getUserInformation "gishi_yama"
  liftIO $ print user'
  ctx3 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx3))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 4. -----------------------------"
  tags <- getTagsAFirstPage

  liftIO $ mapM_ (\page -> print $ (++) "  >>>> " (show page)) (pagenation tags)
  ctx4 <- get

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 5. -----------------------------"
  tags' <- getTagsAWithPage (pagenation tags !! 0)
  liftIO $ mapM_ (\l ->  print $ l) (list tags')
  liftIO $ mapM_ (\l ->  print $ l) (pagenation tags')


runCore2 :: StateT QiitaContext IO ()
runCore2 = do
  let newItem = PostItem { title = "Qiita API on Haskell"
                         , body = "Qiita API on Haskell"
                         , tags = [ PostTag "Haskell" [] ]
                         , private = True
                         , gist = False
                         , tweet = False
                         }
  item <- postItem newItem
  liftIO $ print item

runCore3 :: StateT QiitaContext IO ()
runCore3 = do
  ctx <- get
  json <- liftIO $ getJson
  liftIO $ print json
  req <- parseUrl ("https://qiita.com/api/v1/items?token=" ++ (token $ auth ctx))
           >>= \r -> return $
                    r { requestBody = RequestBodyBS json
                      , method = "POST"
                      , requestHeaders = [ ("content-type", "application/json")
                                         , ("user-agent", "Jabara.Qiita/1.0")
                                         ]
                        }
  res <- withManager (\m -> httpLbs req m)
  liftIO $ print $ responseBody res
  where
    getJson = B.readFile "post.json"

test = do
  cs <- LB.readFile "/Users/jabaraster/temp/temp2.txt"
  let e = decode cs :: Maybe Item
  print e

