{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module AppEnv where
import Protolude
import Whoopr.Basic.Subscription
import Types (User)
import WhooprUtil
import DB
import Whoopr.Basic.RIOMonadListSubscriptions

data AppEnv = AppEnv {
    subscriptions :: MVar [BasicSubscription],
    userDb :: MVar [User],
    eventQueue :: TaskQueueType
}

instance HasUserDB AppEnv where
    type UserDBInst AppEnv = [User]
    getUserDb = userDb

instance HasSubscriptionsList AppEnv BasicSubscription where
    hslGetAllSubscriptions = subscriptions

instance HasWebhooks AppEnv where
    getTaskQueueProducer = eventQueue

runSubscriptionMonad :: AppEnv -> ReaderT AppEnv m a -> m a
runSubscriptionMonad env m = runReaderT m env