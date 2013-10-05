{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Function
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy)
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Types
import System.Locale (defaultTimeLocale)
import System.Timeout
import Web.Welshy
import TX

import Database

------------------------------------------------------------------------------

main :: IO ()
main = do
    db <- openDatabase "todo.log" =<< newDB

    welshy 3000 $ do

        post "/lists" $ do
            list <- liftIO $ persistently db createList
            header hLocation $ listLocation (listId list)
            json list

        get "/lists/:list_id" $ do
            listId <- capture "list_id"
            now <- queryParam "when_updated_after" <|> pass
            list <- timeout (30 * 10^6)  -- 30 seconds
                            (persistently db $ do
                                list <- getList listId
                                liftSTM $ check $ now < listUpdatedAt list
                                return list
                            )
                    `catchIO` \case
                        ListNotFound _ -> halt $ status notFound404
            case list of
                Just l  -> json l
                Nothing -> status $ mkStatus 522 "Connection timed out"

        get "/lists/:list_id" $ do
            listId <- capture "list_id"
            list <- persistently db (getList listId)
                    `catchIO` \case
                        ListNotFound _ -> halt $ status notFound404
            json list

        delete "/lists/:list_id" $ do
            listId <- capture "list_id"
            persistently db (deleteList listId)
                `catchIO` \case
                        ListNotFound _ -> halt $ status notFound404
            status noContent204

        post "/lists/:list_id/items" $ do
            listId <- capture "list_id"
            itemText <- jsonParam "text"
            item <- persistently db (createItem itemText listId)
                    `catchIO` \case
                        ListNotFound _ -> halt $ status notFound404
            header hLocation $ itemLocation listId (itemId item)
            json item

        patch "/lists/:list_id/items/:item_id" $ do
            listId <- capture "list_id"
            itemId <- capture "item_id"
            itemText' <- maybeJsonParam "text"
            itemDone' <- maybeJsonParam "done"
            item' <- persistently db
                        (updateItem itemText' itemDone' itemId listId)
                     `catchIO` \case
                        ListNotFound _   -> halt $ status notFound404
                        ItemNotFound _ _ -> halt $ status notFound404
            json item'

        delete "/lists/:list_id/items/:item_id" $ do
            listId <- capture "list_id"
            itemId <- capture "item_id"
            persistently db (deleteItem itemId listId)
                `catchIO` \case
                        ListNotFound _   -> halt $ status notFound404
                        ItemNotFound _ _ -> halt $ status notFound404
            status noContent204

        get "/" $ do
            file "index.html"

        get "/haskell-todo.js" $ do
            file "haskell-todo.js"

------------------------------------------------------------------------------

-- TODO: move into welshy
-- | Parses standard ISO-8601 dates (e.g. @2013-09-29T10:40Z@).
instance FromText UTCTime where
    fromText = maybe (Left "FromText UTCTime: no parse") Right
             . parseTime defaultTimeLocale "%FT%T%QZ"
             . T.unpack


instance ToJSON (ID a) where toJSON = A.toJSON . show . unID
instance FromText (ID a) where fromText = fmap ID . fromText

instance ToJSON Item where
    toJSON o = A.object [ "item_id" .= itemId o
                        , "text" .= itemText o
                        , "done" .= itemDone o
                        , "created_at" .= itemCreatedAt o
                        , "updated_at" .= itemUpdatedAt o ]

instance ToJSON List where
    toJSON o = A.object [ "list_id" .= listId o
                        , "created_at" .= listCreatedAt o
                        , "updated_at" .= listUpdatedAt o
                        , "items" .= sortedItems ]
        where
            sortedItems = sortBy (compare `on` itemCreatedAt)
                        $ IntMap.elems (listItems o)

listLocation :: ListId -> ByteString
listLocation listId = mconcat [ "/lists/", C.pack (show listId) ]

itemLocation :: ListId -> ItemId -> ByteString
itemLocation listId itemId = mconcat [ "/lists/", C.pack (show listId)
                                     , "/items/", C.pack (show itemId) ]
