{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Database where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Time
import Data.Text (Text)
import Data.Typeable
import Data.SafeCopy
import Data.Serialize
import Data.Word
import System.Random
import TX

------------------------------------------------------------------------------

newtype ID a = ID { unID :: Word64 }
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Random)

instance Show (ID a) where show = show . unID

type ListId = ID List
data List = List { listId :: !ListId
                 , listItems :: IntMap Item
                 , listCreatedAt :: !UTCTime
                 , listUpdatedAt :: !UTCTime
                 }

type ItemId = ID Item
data Item = Item { itemId :: !ItemId
                 , itemText :: !Text
                 , itemDone :: !Bool
                 , itemCreatedAt :: !UTCTime
                 , itemUpdatedAt :: !UTCTime
                 }

data DB = DB { dbLists :: TVar (IntMap (TVar List)) }

newDB :: IO DB
newDB = do
    dbLists <- newTVarIO IntMap.empty
    return DB {..}

data DBException = ListNotFound ListId
                 | ItemNotFound ItemId ListId
                 deriving (Show, Typeable)

instance Exception DBException

------------------------------------------------------------------------------

instance Persistable DB where
    data Update DB = CreateList List
                   | DeleteList ListId
                   | CreateItem Item ListId
                   | UpdateItem (Maybe Text) (Maybe Bool) ItemId ListId
                   | DeleteItem ItemId ListId

    replay (CreateList a) = _createList a
    replay (DeleteList a) = deleteList a
    replay (CreateItem a b) = _createItem a b
    replay (UpdateItem a b c d) = void $ updateItem a b c d
    replay (DeleteItem a b) = deleteItem a b

------------------------------------------------------------------------------

createList :: TX DB List
createList = do
    listId <- unsafeIOToTX randomIO
    now <- unsafeIOToTX getCurrentTime
    let list = List { listItems = IntMap.empty
                    , listCreatedAt = now
                    , listUpdatedAt = now
                    , .. }
    _createList list
    return list

_createList :: List -> TX DB ()
_createList list = do
    DB {..} <- getData
    liftSTM $ do
        lists <- readTVar dbLists
        listVar <- newTVar list
        check $ IntMap.notMember (fromIntegral $ listId list) lists
        let lists' = IntMap.insert (fromIntegral $ listId list) listVar lists
        writeTVar dbLists lists'
    record (CreateList list)

getList :: ListId -> TX DB List
getList = (liftSTM . readTVar =<<) . getListVar

getListVar :: ListId -> TX DB (TVar List)
getListVar listId = do
    DB {..} <- getData
    lists <- liftSTM $ readTVar dbLists
    case IntMap.lookup (fromIntegral listId) lists of
        Just listVar -> return listVar
        Nothing -> throwTX $ ListNotFound listId

deleteList :: ListId -> TX DB ()
deleteList listId = do
    DB {..} <- getData
    lists <- liftSTM $ readTVar dbLists
    when (IntMap.notMember (fromIntegral listId) lists)
         (throwTX $ ListNotFound listId)
    let lists' = IntMap.delete (fromIntegral listId) lists
    liftSTM $ writeTVar dbLists lists'
    record (DeleteList listId)

createItem :: Text -> ListId -> TX DB Item
createItem itemText listId = do
    itemId <- unsafeIOToTX randomIO
    now <- unsafeIOToTX getCurrentTime
    let item = Item { itemDone = False
                    , itemCreatedAt = now
                    , itemUpdatedAt = now
                    , .. }
    _createItem item listId
    return item

_createItem :: Item -> ListId -> TX DB ()
_createItem item listId = do
    listVar <- getListVar listId
    liftSTM $ do
        list <- readTVar listVar
        let items = listItems list
        check $ IntMap.notMember (fromIntegral $ itemId item) items
        let items' = IntMap.insert (fromIntegral $ itemId item) item items
            list' = list { listItems = items'
                         , listUpdatedAt = itemUpdatedAt item }
        writeTVar listVar list'
    record (CreateItem item listId)

updateItem :: Maybe Text -> Maybe Bool -> ItemId -> ListId -> TX DB Item
updateItem itemText' itemDone' itemId listId = do
    listVar <- getListVar listId
    list <- liftSTM $ readTVar listVar
    let items = listItems list
    case IntMap.lookup (fromIntegral itemId) items of
        Nothing -> throwTX $ ItemNotFound itemId listId
        Just item -> do
            now <- unsafeIOToTX getCurrentTime
            let item' = item { itemText = fromMaybe (itemText item) itemText'
                             , itemDone = fromMaybe (itemDone item) itemDone'
                             , itemUpdatedAt = now }
                items' = IntMap.insert (fromIntegral itemId) item' items
                list' = list { listItems = items'
                             , listUpdatedAt = now }
            liftSTM $ writeTVar listVar list'
            record (UpdateItem itemText' itemDone' itemId listId)
            return item

deleteItem :: ItemId -> ListId -> TX DB ()
deleteItem itemId listId = do
    listVar <- getListVar listId
    list <- liftSTM $ readTVar listVar
    when (IntMap.notMember (fromIntegral itemId) (listItems list))
         (throwTX $ ItemNotFound itemId listId)
    now <- unsafeIOToTX getCurrentTime
    let items' = IntMap.delete (fromIntegral itemId) (listItems list)
        list' = list { listItems = items'
                     , listUpdatedAt = now }
    liftSTM $ writeTVar listVar list'
    record (DeleteItem itemId listId)

------------------------------------------------------------------------------

instance SafeCopy (Update DB) where
    putCopy (CreateList a) = contain $ putWord8 0 >> safePut a
    putCopy (DeleteList a) = contain $ putWord8 1 >> safePut a
    putCopy (CreateItem a b) = contain $ putWord8 2 >> safePut a >> safePut b
    putCopy (UpdateItem a b c d) = contain $ putWord8 3 >> safePut a >> safePut b
                                                        >> safePut c >> safePut d
    putCopy (DeleteItem a b) = contain $ putWord8 4 >> safePut a >> safePut b
    getCopy = contain $ do
        tag <- getWord8
        case tag of
            0 -> CreateList <$> safeGet
            1 -> DeleteList <$> safeGet
            2 -> CreateItem <$> safeGet <*> safeGet
            3 -> UpdateItem <$> safeGet <*> safeGet <*> safeGet <*> safeGet
            4 -> DeleteItem <$> safeGet <*> safeGet
            _ -> fail $ "unknown tag \"" ++ show tag ++ "\""


deriveSafeCopy 0 'base ''ID
deriveSafeCopy 0 'base ''List
deriveSafeCopy 0 'base ''Item
