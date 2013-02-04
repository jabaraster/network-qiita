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
        (\ctx -> evalStateT runCoreMats ctx) -- 認証OK後の処理

{- ------------------------------------------
 - 認証なし実行.
------------------------------------------- -}
run2 :: IO ()
run2 = do
  runCoreMats2

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
  liftIO $ mapM_ print (list tags')
  liftIO $ mapM_ print (pagenation tags')


runPostItem :: StateT QiitaContext IO ()
runPostItem = do
  let newItem = PostItem { post_item_title = "Qiita API on Haskell"
                         , post_item_body = "Qiita API on Haskell"
                         , post_item_tags = [ PostTag "Haskell" [] ]
                         , post_item_private = True
                         , post_item_gist = False
                         , post_item_tweet = False
                         }
  item <- postItem newItem
  liftIO $ print item

runCoreMats :: StateT QiitaContext IO ()
runCoreMats = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  items <- getItemsAFirstPage
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  items <- getItemsAFirstPagePerPage 2
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  items <- getItemsAWithPage (pagenation items !! 0)
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

runCoreMats2 :: IO ()
runCoreMats2 = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  items <- getItemsFirstPage
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  items <- getItemsFirstPagePerPage 5
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  items <- getItemsWithPage (pagenation items !! 0)
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)

runGetTagItems :: StateT QiitaContext IO ()
runGetTagItems = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  itemList1 <- liftIO $ getTagItemsFirstPage "Haskell"
  let items1 = fst itemList1
  liftIO $ mapM_ print (list items1)
  liftIO $ mapM_ print (pagenation items1)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  itemList2 <- liftIO $ getTagItemsFirstPage' "Haskell" 10
  let items2 = fst itemList2
  liftIO $ mapM_ print (list items2)
  liftIO $ mapM_ print (pagenation items2)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  itemList3 <- liftIO $ getTagItemsWithPage (pagenation items2 !! 0)
  let items3 = fst itemList3
  liftIO $ mapM_ print (list items3)
  liftIO $ mapM_ print (pagenation items3)

runUpdateItem :: StateT QiitaContext IO ()
runUpdateItem = do
  let newItem = PostItem { post_item_title = "Qiita API on Haskell"
                         , post_item_body = "Qiita API on Haskell"
                         , post_item_tags = [ PostTag "Haskell" [] ]
                         , post_item_private = True
                         , post_item_gist = False
                         , post_item_tweet = False
                         }
  postedS <- postItem newItem

  case postedS of
    Left     err -> liftIO $ print err
    Right posted -> do
      let updated = itemToUpdateItem posted
      updatedS <- updateItem updated {
                    update_item_title = "Updated Item."
                  }
      liftIO $ print updatedS

