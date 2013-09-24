{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class
import Control.Exception
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

liftTX :: (Exception e, MonadIO m) => Database d -> TX d a -> (e -> m a) -> m a
liftTX d tx h = either h return =<< (liftIO $ try $ persistently d tx)

liftTX_ :: MonadIO m => Database d -> TX d a -> m a
liftTX_ d tx = liftIO $ persistently d tx

main :: IO ()
main = do
    db <- openDatabase "todo.log" =<< newDB

    welshy 3000 $ do

        post "/lists" $ do
            list <- liftTX_ db createList
            header hLocation $ listLocation (listId list)
            json list

        get "/lists/:list_id" $ do
            listId <- capture "list_id"
            list <- liftTX db (getList listId)
                  $ \case
                        ListNotFound _ -> halt $ status notFound404
            json list

        delete "/lists/:list_id" $ do
            listId <- capture "list_id"
            liftTX db (deleteList listId)
                $ \case
                        ListNotFound _ -> halt $ status notFound404
            status noContent204

        post "/lists/:list_id/items" $ do
            listId <- capture "list_id"
            itemText <- jsonParam "text"
            item <- liftTX db (createItem itemText listId)
                  $ \case
                        ListNotFound _ -> halt $ status notFound404
            header hLocation $ itemLocation listId (itemId item)
            json item

        patch "/lists/:list_id/items/:item_id" $ do
            listId <- capture "list_id"
            itemId <- capture "item_id"
            itemText' <- maybeJsonParam "text"
            itemDone' <- maybeJsonParam "done"
            item' <- liftTX db (updateItem itemText' itemDone' itemId listId)
                   $ \case
                        ListNotFound _   -> halt $ status notFound404
                        ItemNotFound _ _ -> halt $ status notFound404
            json item'

        delete "/lists/:list_id/items/:item_id" $ do
            listId <- capture "list_id"
            itemId <- capture "item_id"
            liftTX db (deleteItem itemId listId)
                $ \case
                        ListNotFound _   -> halt $ status notFound404
                        ItemNotFound _ _ -> halt $ status notFound404
            status noContent204

------------------------------------------------------------------------------

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
