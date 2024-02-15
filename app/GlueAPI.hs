{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GlueAPI where

import Protolude
import Servant.API
import Servant
import Types
import Data.Time (getCurrentTime)
import Data.Aeson.Key (toText)
import Whoopr.Basic.SubscriptionAPI
import DB (HasUserDB (..))
import Network.Wai (Request)
import Whoopr.Basic.RIOMonadListSubscriptions (HasSubscriptionsList (..))
import Whoopr.Basic.Subscription (BasicSubscription)
import WhooprUtil
import UserAPI (userApp)
import AppEnv

type GlueAPI = "service" :> Raw
         :<|>  "webhooks" :> Raw

glueApiProxy :: Proxy GlueAPI
glueApiProxy = Proxy

glueApiServer :: AppEnv -> Server GlueAPI
glueApiServer env = Tagged (userApp env) :<|> Tagged (subscriptionsApp $ runSubscriptionMonad env)

app :: AppEnv -> Application
app env = serve glueApiProxy (glueApiServer env)