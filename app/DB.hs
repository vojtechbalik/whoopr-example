{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module DB where

import Protolude
import Types

class Traversable (TraversableHolder a) => UserDB a where
    type TraversableHolder a :: * -> *
    getAllUsers :: a -> TraversableHolder a User
    getUser     :: a -> ID -> Maybe User
    getUser db needle = find (\User{id=haystack} -> haystack == needle) $ getAllUsers db
    storeUser   :: a -> User -> (Bool, a)
    getVacantID :: a -> ID
    deleteUser  :: a -> ID   -> (Bool, a)

instance UserDB [User] where
    type TraversableHolder [User] = []
    getAllUsers = identity
    getVacantID [] = 0
    getVacantID db = let User{id=maxId} = maximumBy (compare `on` (\User{id=id} -> id)) db
                     in  maxId + 1
    storeUser db u@User{id=id} = case getUser db id of
        Just _  -> (False, db  )
        Nothing -> (True , u:db)
    deleteUser db needle = foldr
        (\u@User{id=haystack} (found, acc) -> if needle == haystack then (True, acc) else (found, u:acc))
        (False, [])
        db

class (UserDB (UserDBInst env)) => HasUserDB env where
    type UserDBInst env :: *
    getUserDb :: env -> MVar (UserDBInst env)

type MonadUserDB env m = (MonadReader env m, MonadIO m, HasUserDB env)

readDB :: (MonadUserDB env m) => m (UserDBInst env)
readDB = ask <&> getUserDb >>= liftIO . readMVar

-- Atomically modifies DB. Wrapped value signifies whether the operation was succesful.
modifyDB :: (MonadUserDB env m) => (UserDBInst env -> (Bool, UserDBInst env)) -> m Bool
modifyDB f = do
    db <- ask <&> getUserDb >>= liftIO . takeMVar
    let (result, db') = f db
    ask <&> getUserDb >>= liftIO . (`putMVar` db')
    return result

getAllUsersM :: MonadUserDB env m => m (TraversableHolder (UserDBInst env) User)
getAllUsersM = readDB <&> getAllUsers

getUserM :: MonadUserDB udb m => ID -> m (Maybe User)
getUserM id = do
    db <- readDB
    return $ getUser db id

getVacantIdM :: MonadUserDB udb m => m ID
getVacantIdM = readDB <&> getVacantID

storeUserM :: MonadUserDB udb m => User -> m Bool
storeUserM u = modifyDB (`storeUser` u)

deleteUserM :: MonadUserDB udb m => ID -> m Bool
deleteUserM id = modifyDB (`deleteUser` id)
