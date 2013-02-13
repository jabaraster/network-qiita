# Network.Qiita
This package is __Haskell wrapper__ of __[Qiita][]__ 's WebAPI.

# Usage

## Login

Function ```withAuthentication``` for login and after operation.

example)

```Haskell
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit)
        (\ctx -> evalStateT runGetUserItemsCore ctx)
```

```Haskell
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
```

If Function return ```StateT QiitaContext IO a```, this function require __login token__.

[Qiita]: <http://qiita.com>
