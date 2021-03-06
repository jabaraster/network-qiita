{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Qiita.Types (
  QiitaError(..)
  , Auth(..)
  , RateLimit(..)
  , User(..)
  , Tag(..)
  , TagA(..)
  , FollowingTag(..)
  , FollowingUser(..)
  , PostItem(..)
  , UpdateItem(..)
  , PostTag(..)
  , QiitaContext(..)
  , Pagenation(..)
  , ListData(..)
  , ItemUser(..)
  , ItemTag(..)
  , Item(..)
  , FullItem(..)
  , UserName
  , Password
  , PerPage
  , TagName
  , ItemUuid
  , Q
  , itemToUpdateItem
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
data User = User { user_name :: String
                 , user_url_name :: String
                 , user_profile_image_url :: String
                 , user_url :: String
                 , user_description :: String
                 , user_website_url :: String
                 , user_organization :: String
                 , user_location :: String
                 , user_facebook :: String
                 , user_linkedin :: String
                 , user_twitter :: String
                 , user_github :: String
                 , user_followers :: Int
                 , user_following_users :: Int
                 , user_items :: Int
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
--
-- 特定のユーザーがフォローしているタグ情報.
--
data FollowingTag = FollowingTag { following_tag_name :: String
                 , following_tag_url_name :: String
                 , following_tag_icon_url :: String
                 } deriving (Show, Eq)

--
-- 特定のユーザーがフォローしているユーザー情報.
--
data FollowingUser = FollowingUser { following_user_name :: String
                 , following_user_url_name :: String
                 , following_user_profile_image_url :: String
                 } deriving (Show, Eq)

data ListData a = ListData { list :: [a], pagenation :: [Pagenation] }
                  deriving (Show, Eq)

data PostItem = PostItem { post_item_title :: String
                         , post_item_body :: String
                         , post_item_tags :: [PostTag]
                         , post_item_private :: Bool
                         , post_item_gist :: Bool
                         , post_item_tweet :: Bool
                         } deriving (Show, Eq)

data UpdateItem = UpdateItem { update_item_uuid :: String
                             , update_item_title :: String
                             , update_item_tags :: [PostTag]
                             , update_item_body :: String
                             , update_item_private :: Bool
                             } deriving (Show, Eq)

data PostTag = PostTag { post_tag_name :: String
                       , post_tag_versions :: [String]
                       } deriving (Show, Eq)

data ItemUser = ItemUser { item_user_name :: String
                         , item_user_url_name :: String
                         , item_user_profile_image_url :: String
                         } deriving (Show, Eq)
data UserComment = UserComment { user_comment_name :: String
                               , user_comment_url_name :: String
                               , user_comment_profile_image_url :: String
                               , user_comment_body :: String
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
data ItemComment = ItemComment { item_comment_id :: Integer
                               , item_comment_uuid :: String
                               , item_comment_user :: [UserComment]
                               } deriving (Show, Eq)
data FullItem =  FullItem { full_item_id :: Integer
                          , full_item_uuid :: String
                          , full_item_user :: ItemUser
                          , full_item_title :: String
                          , full_item_body :: String
                          , full_item_raw_body :: String
                          , full_item_created_at :: String
                          , full_item_created_at_seconds :: Integer
                          , full_item_updated_at :: String
                          , full_item_created_at_in_words :: String
                          , full_item_updated_at_in_words :: String
                          , full_item_tags :: [ItemTag]
                          , full_item_stock_count :: Integer
                          , full_item_stock_users :: [String]
                          , full_item_comment_count :: Integer
                          , full_item_url :: String
                          , full_item_gist_url :: Maybe String
                          , full_item_tweet :: Bool
                          , full_item_private :: Bool
                          , full_item_comments :: [ItemComment]
                          } deriving (Show, Eq)

{- ------------------------------------------
 - type alias.
------------------------------------------- -}
type UserName = BS.ByteString
type Password = BS.ByteString
type PerPage = Int
type TagName = BS.ByteString
type ItemUuid = BS.ByteString
type Q = BS.ByteString

{- ------------------------------------------
 - instance difinitions.
------------------------------------------- -}

instance FromJSON QiitaError
instance FromJSON Auth
instance FromJSON RateLimit

instance FromJSON User where
  parseJSON (Object v) = User <$>
                           v .: "name"
                           <*> v .: "url_name"
                           <*> v .: "profile_image_url"
                           <*> v .: "url"
                           <*> v .: "description"
                           <*> v .: "website_url"
                           <*> v .: "organization"
                           <*> v .: "location"
                           <*> v .: "facebook"
                           <*> v .: "linkedin"
                           <*> v .: "twitter"
                           <*> v .: "github"
                           <*> v .: "followers"
                           <*> v .: "following_users"
                           <*> v .: "items"
  parseJSON _          = mzero

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
  toJSON pi = object [ "title" .= post_item_title pi
                     , "body" .= post_item_body pi
                     , "tags" .= (buildTags $ post_item_tags pi)
                     , "private" .= post_item_private pi
                     , "gist" .= post_item_gist pi
                     , "tweet" .= post_item_tweet pi
                     ]

instance ToJSON UpdateItem where
  toJSON ui = object [ "title" .= update_item_title ui
                     , "tags" .= (buildTags $ update_item_tags ui)
                     , "body" .= update_item_body ui
                     , "private" .= update_item_private ui
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

instance FromJSON FollowingTag where
  parseJSON (Object v) = FollowingTag <$>
                            v .: "name"
                            <*> v .: "url_name"
                            <*> v .: "icon_url"
  parseJSON _          = mzero

instance FromJSON FollowingUser where
  parseJSON (Object v) = FollowingUser <$>
                           v .: "name"
                           <*> v .: "url_name"
                           <*> v .: "profile_image_url"
  parseJSON _          = mzero

instance FromJSON FullItem where
  parseJSON (Object v) = FullItem <$>
                               v .: "id"
                           <*> v .: "uuid"
                           <*> v .: "user"
                           <*> v .: "title"
                           <*> v .: "body"
                           <*> v .: "raw_body"
                           <*> v .: "created_at"
                           <*> v .: "created_at_as_seconds"
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
                           <*> v .: "comments"
  parseJSON _          = mzero
instance FromJSON ItemComment where
  parseJSON (Object v ) =  ItemComment <$>
                                 v .: "id"
                             <*> v .: "uuid"
                             <*> v .: "user"
  parseJSON _           = mzero
instance FromJSON UserComment where
  parseJSON (Object v) = UserComment <$>
                               v .: "name"
                           <*> v .: "url_name"
                           <*> v .: "profile_image_url"
                           <*> v .: "body"
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

{- ------------------------------------------
 - 型変換用関数
------------------------------------------- -}
itemToUpdateItem :: Item -> UpdateItem
itemToUpdateItem item = UpdateItem {
                          update_item_uuid = item_uuid item
                          , update_item_title = item_title item
                          , update_item_tags = map itemTagToPostTag $ item_tags item
                          , update_item_body = item_body item
                          , update_item_private = item_private item
                        }
  where
    itemTagToPostTag itemTag = PostTag {
                                 post_tag_name = item_tag_name itemTag
                                 , post_tag_versions = item_tag_versions itemTag
                               }

{- ------------------------------------------
 - Private functions.
------------------------------------------- -}
buildTags tags = V.fromList $ Prelude.map tagToObject tags
  where
    tagToObject tag = object [ "name" .= post_tag_name tag
                             , "versions" .= (V.fromList $ post_tag_versions tag)
                             ]

