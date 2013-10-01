{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.Monoid
import Network.HTTP.Types
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

------------------------------------------------------------------------------

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
                        , "items" .= IntMap.elems (listItems o) ]

listLocation :: ListId -> ByteString
listLocation listId = mconcat [ "/lists/", C.pack (show listId) ]

itemLocation :: ListId -> ItemId -> ByteString
itemLocation listId itemId = mconcat [ "/lists/", C.pack (show listId)
                                     , "/items/", C.pack (show itemId) ]
