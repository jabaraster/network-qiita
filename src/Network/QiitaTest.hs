{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.QiitaTest where

import Network.Qiita

import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC


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
  user' <- liftIO $ getUserInformation "jabaraster"
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

runGetItems :: IO ()
runGetItems = do
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runGetItemsCore ctx) -- 認証OK後の処理

  -- 認証なし実行
  runGetItemsCore2

runGetItemsCore :: StateT QiitaContext IO ()
runGetItemsCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  items <- getItemsAFirstPage
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  items <- getItemsAFirstPage' 2
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

runGetItemsCore2 :: IO ()
runGetItemsCore2 = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  items <- getItemsFirstPage
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  items <- getItemsFirstPage' 5
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  items <- getItemsWithPage (pagenation items !! 0)
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)

runGetStocks :: IO ()
runGetStocks = do
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runGetStocksCore ctx) -- 認証OK後の処理

runGetStocksCore :: StateT QiitaContext IO ()
runGetStocksCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  items <- getStocksAFirstPage
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  items <- getStocksAFirstPage' 2
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  items <- getStocksAWithPage (pagenation items !! 0)
  liftIO $ mapM_ (\l ->  print $ l) (list items)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

runGetUserFollowingTags :: IO ()
runGetUserFollowingTags = do
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runGetUserFollowingTagsCore ctx) -- 認証OK後の処理

  -- 認証なし実行
  runGetUserFollowingTagsCore2

runGetUserFollowingTagsCore :: StateT QiitaContext IO ()
runGetUserFollowingTagsCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  tagList1 <- liftIO $ getUserFollowingTagsAFirstPage "jabaraster"
  let tags1 = fst tagList1
  liftIO $ mapM_ (\l ->  print $ l) (list tags1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation tags1)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  tagList2 <- liftIO $ getUserFollowingTagsAFirstPage' "jabaraster" 2
  let tags2 = fst tagList2
  liftIO $ mapM_ (\l ->  print $ l) (list tags2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation tags2)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  tagList3 <- liftIO $ getUserFollowingTagsAWithPage (pagenation tags2 !! 0)
  let tags3 = fst tagList3
  liftIO $ mapM_ (\l ->  print $ l) (list tags3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation tags3)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

runGetUserFollowingTagsCore2 :: IO ()
runGetUserFollowingTagsCore2 = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  tagList1 <- liftIO $ getUserFollowingTagsFirstPage "jabaraster"
  let tags1 = fst tagList1
  liftIO $ mapM_ (\l ->  print $ l) (list tags1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation tags1)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  tagList2 <- liftIO $ getUserFollowingTagsFirstPage' "jabaraster" 2
  let tags2 = fst tagList2
  liftIO $ mapM_ (\l ->  print $ l) (list tags2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation tags2)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  tagList3 <- liftIO $ getUserFollowingTagsWithPage (pagenation tags2 !! 0)
  let tags3 = fst tagList3
  liftIO $ mapM_ (\l ->  print $ l) (list tags3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation tags3)

runGetUserFollowingUsers :: IO ()
runGetUserFollowingUsers = do
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runGetUserFollowingUsersCore ctx) -- 認証OK後の処理

  -- 認証なし実行
  runGetUserFollowingUsersCore2

runGetUserFollowingUsersCore :: StateT QiitaContext IO ()
runGetUserFollowingUsersCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  userList1 <- liftIO $ getUserFollowingUsersAFirstPage "jabaraster"
  let users1 = fst userList1
  liftIO $ mapM_ (\l ->  print $ l) (list users1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation users1)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  userList2 <- liftIO $ getUserFollowingUsersAFirstPage' "jabaraster" 2
  let users2 = fst userList2
  liftIO $ mapM_ (\l ->  print $ l) (list users2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation users2)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  userList3 <- liftIO $ getUserFollowingUsersAWithPage (pagenation users2 !! 0)
  let users3 = fst userList3
  liftIO $ mapM_ (\l ->  print $ l) (list users3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation users3)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

runGetUserFollowingUsersCore2 :: IO ()
runGetUserFollowingUsersCore2 = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  userList1 <- liftIO $ getUserFollowingUsersFirstPage "jabaraster"
  let users1 = fst userList1
  liftIO $ mapM_ (\l ->  print $ l) (list users1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation users1)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  userList2 <- liftIO $ getUserFollowingUsersFirstPage' "jabaraster" 2
  let users2 = fst userList2
  liftIO $ mapM_ (\l ->  print $ l) (list users2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation users2)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  userList3 <- liftIO $ getUserFollowingUsersWithPage (pagenation users2 !! 0)
  let users3 = fst userList3
  liftIO $ mapM_ (\l ->  print $ l) (list users3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation users3)

runGetUserStocks :: IO ()
runGetUserStocks = do
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runGetUserStocksCore ctx) -- 認証OK後の処理

  -- 認証なし実行
  runGetUserStocksCore2

runGetUserStocksCore :: StateT QiitaContext IO ()
runGetUserStocksCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  itemList1 <- liftIO $ getUserStocksAFirstPage "matscity@github"
  let items1 = fst itemList1
  liftIO $ mapM_ (\l ->  print $ l) (list items1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items1)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  itemList2 <- liftIO $ getUserStocksAFirstPage' "matscity@github" 2
  let items2 = fst itemList2
  liftIO $ mapM_ (\l ->  print $ l) (list items2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items2)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  itemList3 <- liftIO $ getUserStocksAWithPage (pagenation items2 !! 0)
  let items3 = fst itemList3
  liftIO $ mapM_ (\l ->  print $ l) (list items3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items3)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

runGetUserStocksCore2 :: IO ()
runGetUserStocksCore2 = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  itemList1 <- liftIO $ getUserStocksFirstPage "jabaraster"
  let items1 = fst itemList1
  liftIO $ mapM_ (\l ->  print $ l) (list items1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items1)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  itemList2 <- liftIO $ getUserStocksFirstPage' "jabaraster" 2
  let items2 = fst itemList2
  liftIO $ mapM_ (\l ->  print $ l) (list items2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items2)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  itemList3 <- liftIO $ getUserStocksWithPage (pagenation items2 !! 0)
  let items3 = fst itemList3
  liftIO $ mapM_ (\l ->  print $ l) (list items3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items3)

runGetUserItems :: IO ()
runGetUserItems = do
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runGetUserItemsCore ctx) -- 認証OK後の処理

  -- 認証なし実行
  runGetUserItemsCore2

runGetUserItemsCore :: StateT QiitaContext IO ()
runGetUserItemsCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  itemList1 <- liftIO $ getUserItemsAFirstPage "jabaraster"
  let items1 = fst itemList1
  liftIO $ mapM_ (\l ->  print $ l) (list items1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items1)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  itemList2 <- liftIO $ getUserItemsAFirstPage' "jabaraster" 2
  let items2 = fst itemList2
  liftIO $ mapM_ (\l ->  print $ l) (list items2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items2)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  itemList3 <- liftIO $ getUserItemsAWithPage (pagenation items2 !! 0)
  let items3 = fst itemList3
  liftIO $ mapM_ (\l ->  print $ l) (list items3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items3)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

runGetUserItemsCore2 :: IO ()
runGetUserItemsCore2 = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  itemList1 <- liftIO $ getUserItemsFirstPage "jabaraster"
  let items1 = fst itemList1
  liftIO $ mapM_ (\l ->  print $ l) (list items1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items1)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  itemList2 <- liftIO $ getUserItemsFirstPage' "jabaraster" 2
  let items2 = fst itemList2
  liftIO $ mapM_ (\l ->  print $ l) (list items2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items2)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  itemList3 <- liftIO $ getUserItemsWithPage (pagenation items2 !! 0)
  let items3 = fst itemList3
  liftIO $ mapM_ (\l ->  print $ l) (list items3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items3)

runSearchItems :: IO ()
runSearchItems = do
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runSearchItemsCore ctx) -- 認証OK後の処理

  -- 認証なし実行
  runSearchItemsCore2

runSearchItemsCore :: StateT QiitaContext IO ()
runSearchItemsCore = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 0. -----------------------------"
  items0 <- searchStockedItemsAFirstPage "java"
  liftIO $ mapM_ (\l ->  print $ l) (list items0)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items0)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  itemList1 <- liftIO $ searchItemsAFirstPage "ruby emacs"
  let items1 = fst itemList1
  liftIO $ mapM_ (\l ->  print $ l) (list items1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items1)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  itemList2 <- liftIO $ searchItemsAFirstPage' "ruby emacs" 2
  let items2 = fst itemList2
  liftIO $ mapM_ (\l ->  print $ l) (list items2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items2)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  itemList3 <- liftIO $ searchItemsAWithPage (pagenation items2 !! 0)
  let items3 = fst itemList3
  liftIO $ mapM_ (\l ->  print $ l) (list items3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items3)
  ctx2 <- get
  liftIO $ putStrLn ("Post: " ++ (show ctx2))

runSearchItemsCore2 :: IO ()
runSearchItemsCore2 = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 1. -----------------------------"
  itemList1 <- liftIO $ searchItemsFirstPage "haskell"
  let items1 = fst itemList1
  liftIO $ mapM_ (\l ->  print $ l) (list items1)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items1)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 2. -----------------------------"
  itemList2 <- liftIO $ searchItemsFirstPage' "haskell" 2
  let items2 = fst itemList2
  liftIO $ mapM_ (\l ->  print $ l) (list items2)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items2)

  liftIO $ putStrLn ""
  liftIO $ putStrLn "- 3. -----------------------------"
  itemList3 <- liftIO $ searchItemsWithPage (pagenation items2 !! 0)
  let items3 = fst itemList3
  liftIO $ mapM_ (\l ->  print $ l) (list items3)
  liftIO $ mapM_ (\l ->  print $ l) (pagenation items3)

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

runUpdateItem :: IO ()
runUpdateItem = do
  putStrLn "ユーザ名を入力してEnter、次にパスワードを入力してEnterを押すこと！"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT runUpdateItemCore ctx) -- 認証OK後の処理

runUpdateItemCore :: StateT QiitaContext IO ()
runUpdateItemCore = do
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

runDeleteItem :: IO ()
runDeleteItem = do
  putStrLn "ユーザ名を入力してEnter、次にパスワードを入力してEnter、最後に削除したい投稿のUUIDを入力してEnterを押すこと！"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1
  let uuid = C8.pack $ lines !! 2

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT (runDeleteItemCore uuid) ctx) -- 認証OK後の処理

runDeleteItemCore :: B.ByteString -> StateT QiitaContext IO ()
runDeleteItemCore uuid = do
  deleteItem uuid
  return ()

runStockItem :: IO ()
runStockItem = do
  putStrLn "ユーザ名を入力してEnter、次にパスワードを入力してEnter、最後にストックしたい投稿のUUIDを入力してEnterを押すこと！"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1
  let uuid = C8.pack $ lines !! 2

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT (runStockItemCore uuid) ctx) -- 認証OK後の処理

runStockItemCore :: ItemUuid -> StateT QiitaContext IO ()
runStockItemCore uuid = do
  stockItem uuid
  return ()

runUnstockItem :: IO ()
runUnstockItem = do
  putStrLn "ユーザ名を入力してEnter、次にパスワードを入力してEnter、最後にストック解除したい投稿のUUIDを入力してEnterを押すこと！"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1
  let uuid = C8.pack $ lines !! 2

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT (runUnstockItemCore uuid) ctx) -- 認証OK後の処理

runUnstockItemCore :: ItemUuid -> StateT QiitaContext IO ()
runUnstockItemCore uuid = do
  unstockItem uuid
  return ()

runGetItem :: IO ()
runGetItem = do
  putStrLn "ユーザ名を入力してEnter、次にパスワードを入力してEnter、最後に取得したい投稿のUUIDを入力してEnterを押すこと！"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1
  let uuid = C8.pack $ lines !! 2

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
       (\ctx -> evalStateT (runGetItemCore uuid) ctx) -- 認証OK後の処理

runGetItemCore :: ItemUuid -> StateT QiitaContext IO ()
runGetItemCore uuid = do
  item <- liftIO $ getItem uuid
  liftIO $ print item
