# Network.Qiita

# Specification

## IO a


## StateT QiitaContext IO a
token required.

# Usage

## Login

```Haskell
  putStrLn "Input your User Id and Enter、then password and Enter"
  input <- getContents
  lines <- return $ lines input

  let user = C8.pack $ lines !! 0
  let pass = C8.pack $ lines !! 1

  withAuthentication user pass
        (\err limit -> print err >> print limit) -- 認証エラー時の処理
        (\ctx -> evalStateT runGetUserItemsCore ctx) -- 認証OK後の処理
```
