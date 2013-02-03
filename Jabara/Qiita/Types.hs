{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jabara.Qiita.Types (
  QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  , Tag(..)
  , TagA(..)
  , PostItem(..)
  , PostTag(..)
  , QiitaContext(..)
  , Pagenation(..)
  , ListData(..)
  , ItemUser(..)
  , ItemTag(..)
  , Item(..)
  , UserName
  , Password
  , PerPage
  , TagName
  ) where

import Control.Applicative ((<*>), (<|>), (<$>))
import Control.Monad.State(mzero)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import GHC.Generics (Generic)

{- ------------------------------------------
 - data difinitions.
------------------------------------------- -}

data QiitaError = QiitaError { errorMessage :: String }
                  deriving (Show, Generic)
data Auth = Auth { token :: String }
            deriving (Show, Eq, Generic)
data RateLimit = RateLimit {remaining :: Int, limit :: Int }
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
data QiitaContext = QiitaContext { auth :: Auth, rateLimit :: RateLimit }
                    deriving (Show, Eq, Generic)
data Pagenation = Pagenation { pageRel :: BS.ByteString, pageUrl :: BS.ByteString }
                  deriving (Show, Eq)
--
-- 未認証な状態で取得可能なタグ情報.
--
data Tag = Tag { tag_name :: String
                 , tag_url_name :: String
                 , tag_icon_url :: String
                 , tag_item_count :: Int
                 , tag_follower_count :: Int
               } deriving (Show, Eq)
--
-- 認証済みユーザが取得可能なタグ情報.
--
data TagA = TagA { taga_name :: String
                 , taga_url_name :: String
                 , taga_icon_url :: String
                 , taga_item_count :: Int
                 , taga_follower_count :: Int
                 , taga_following :: Bool
               } deriving (Show, Eq)
data ListData a = ListData { list :: [a], pagenation :: [Pagenation] }
                  deriving (Show, Eq)

data PostItem = PostItem { title :: String
                         , body :: String
                         , tags :: [PostTag]
                         , private :: Bool
                         , gist :: Bool
                         , tweet :: Bool
                         } deriving (Show, Eq)

data PostTag = PostTag { post_tag_name :: String
                       , post_tag_versions :: [String]
                       } deriving (Show, Eq)

data ItemUser = ItemUser { item_user_name :: String
                         , item_user_url_name :: String
                         , item_user_profile_image_url :: String
                         } deriving (Show, Eq)
data ItemTag = ItemTag { item_tag_name :: String
                       , item_tag_url_name :: String
                       , item_tag_icon_url :: String
                       , item_tag_versions :: [String]
                       } deriving (Show, Eq)
data Item = Item { item_id :: Integer
                 , item_uuid :: String
                 , item_user :: ItemUser
                 , item_title :: String
                 , item_body :: String
                 , item_created_at :: String -- TODO 相応しいデータ型
                 , item_updated_at :: String -- TODO
                 , item_created_at_in_words :: String
                 , item_updated_at_in_words :: String
                 , item_tags :: [ItemTag]
                 , item_stock_count :: Integer
                 , item_stock_users :: [String]
                 , item_comment_count :: Integer
                 , item_url :: String
                 , item_gist_url :: Maybe String
                 , item_tweet :: Bool
                 , item_private :: Bool
                 } deriving (Show, Eq)

{- ------------------------------------------
 - type alias.
------------------------------------------- -}
type UserName = BS.ByteString
type Password = BS.ByteString
type PerPage = Int
type TagName = BS.ByteString

{- ------------------------------------------
 - instance difinitions.
------------------------------------------- -}

instance FromJSON QiitaError
instance FromJSON Auth
instance FromJSON RateLimit
instance FromJSON User

-- 自動パースにたよるとセレクタ(＝プロパティ)の名前がかぶってしまう.
-- Haskellでこれは許されないので、手動でパースを書かざるを得ない.
instance FromJSON Tag where
  parseJSON (Object v) = Tag <$>
                            v .: "name"
                            <*> v .: "url_name"
                            <*> v .: "icon_url"
                            <*> v .: "item_count"
                            <*> v .: "follower_count"
  parseJSON _          = mzero

instance ToJSON PostItem where
  toJSON pi = object [
                "title" .= title pi
                , "body" .= body pi
                , "tags" .= buildTags
                , "private" .= private pi
                , "gist" .= gist pi
                , "tweet" .= tweet pi
              ]
    where
      buildTags = V.fromList $ Prelude.map tagToObject $ tags pi
      tagToObject tag = object [ "name" .= post_tag_name tag
                               , "versions" .= (V.fromList $ post_tag_versions tag)
                               ]
instance FromJSON TagA where
  parseJSON (Object v) = TagA <$>
                            v .: "name"
                            <*> v .: "url_name"
                            <*> v .: "icon_url"
                            <*> v .: "item_count"
                            <*> v .: "follower_count"
                            <*> v .: "following"
  parseJSON _          = mzero

{- ------------------------------------------
 - ItemをJSONに変換するロジック.
 - プロパティが多くなってきたときにこの表記法はかなりイヤなのだが、
 - 他に良い書き方を調査する時間がない・・・
------------------------------------------- -}
instance FromJSON Item where
  parseJSON (Object v) = Item <$>
                         v .: "id"
                         <*> v .: "uuid"
                         <*> v .: "user"
                         <*> v .: "title"
                         <*> v .: "body"
                         <*> v .: "created_at"
                         <*> v .: "updated_at"
                         <*> v .: "created_at_in_words"
                         <*> v .: "updated_at_in_words"
                         <*> v .: "tags"
                         <*> v .: "stock_count"
                         <*> v .: "stock_users"
                         <*> v .: "comment_count"
                         <*> v .: "url"
                         <*> v .: "gist_url"
                         <*> v .: "tweet"
                         <*> v .: "private"
  parseJSON _          = mzero
instance FromJSON ItemUser where
  parseJSON (Object v) = ItemUser <$>
                         v .: "name"
                         <*> v .: "url_name"
                         <*> v .: "profile_image_url"
  parseJSON _          = mzero
instance FromJSON ItemTag where
  parseJSON (Object v) = ItemTag <$>
                         v .: "name"
                         <*> v .: "url_name"
                         <*> v .: "icon_url"
                         <*> v .: "versions"
  parseJSON _          = mzero
