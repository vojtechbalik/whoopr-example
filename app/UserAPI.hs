{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module UserAPI where

import Protolude
import Servant.API
import Servant
import Types
import Data.Time (getCurrentTime)
import DB (
    MonadUserDB, getAllUsersM, storeUserM, getVacantIdM, getUserM, deleteUserM,
    UserDB (..),
    HasUserDB (..))
import Data.Aeson.Key (toText)
import WhooprUtil
import Data.Aeson.Types (object)
import Data.Aeson ((.=))

type UsersAPI = "users" :> ( Get '[JSON] [User]
                        :<|> ReqBody '[JSON] UserCredentials :> PostNoContent )
           :<|> "user"  :> Capture "userid" ID :>   (Get '[JSON] User
                                               :<|>  DeleteNoContent
                                               :<|>  ReqBody '[JSON] UserCredentials :> PutNoContent)

usersApiProxy :: Proxy UsersAPI
usersApiProxy = Proxy

usersApiServer :: (HasUserDB env, HasWebhooks env) => env -> Server UsersAPI
usersApiServer env = hoistServer usersApiProxy (stateToHandler env) usersApiServerImpl

stateToHandler :: (HasUserDB env) => env -> ReaderT env Servant.Handler a -> Servant.Handler a
stateToHandler env m = runReaderT m env

usersApiServerImpl :: (HasUserDB env, HasWebhooks env) => ServerT UsersAPI (ReaderT env Servant.Handler)
usersApiServerImpl = (getAllUsersServer :<|> postUserServer)
                :<|> (\id -> getUserServer id :<|>  deleteUserServer id :<|> updateUserServer id)

getAllUsersServer :: (MonadUserDB udb m) => m [User]
getAllUsersServer = toList <$> getAllUsersM

postUserServer :: (MonadWebhooks env m, MonadUserDB env m, MonadIO m) => UserCredentials -> m NoContent
postUserServer creds = do
    time <- liftIO getCurrentTime
    vacantId <- getVacantIdM
    let user = User {
        id = vacantId,
        credentials = creds,
        account_created = time,
        last_seen = time
    }
    wasStored <- storeUserM user
    when wasStored $ notify "userCreated" user
    return NoContent

getUserServer :: (MonadUserDB env m, MonadError ServerError m) => ID -> m User
getUserServer id = do
    maybeUser <- getUserM id
    case maybeUser of
        Just u  -> return u
        Nothing -> throwError err404

deleteUserServer :: (MonadWebhooks env m, MonadUserDB env m, MonadError ServerError m) => ID -> m NoContent
deleteUserServer id = do
    maybeUser <- getUserM id
    deleteUserM id
    case maybeUser of
        Just u -> notify "userDeleted" u >> return NoContent
        Nothing -> throwError err404

updateUserServer :: (MonadWebhooks env m, MonadUserDB env m, MonadError ServerError m) => ID -> UserCredentials -> m NoContent
updateUserServer id uc = do
    maybeUser <- getUserM id
    user <- case maybeUser of
        Nothing   -> throwError err404
        Just user -> return user
    let updatedUser = user{credentials=uc}
    deleteUserM id
    storeUserM updatedUser
    notify "userUpdated" $ object [ "id" .= id ]
    return NoContent

userApp :: (HasUserDB env, HasWebhooks env) => env -> Application
userApp env = serve usersApiProxy (usersApiServer env)
