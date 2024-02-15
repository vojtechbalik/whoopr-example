{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Protolude
import Types
import Data.Time (getCurrentTime)
import GlueAPI
import Control.Concurrent (forkFinally)
import AppEnv
import Whoopr.EventSender (consumeTaskQueue)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn ("running" :: Text)
    db <- mkDb >>= newMVar
    subscrs <- newMVar []
    ch <- newChan
    let env = AppEnv {
        subscriptions = subscrs,
        userDb = db,
        eventQueue = ch
    }
    forkFinally (forever $ consumeTaskQueue' env) (\case
        Right _ -> print "sender finished OK"
        Left e -> print $ "sender finished with error: " ++ show e)
    run 8082 $ app env

consumeTaskQueue' env@AppEnv{..} = consumeTaskQueue 
    (Proxy :: Proxy ByteString) 
    (runSubscriptionMonad env)
    eventQueue
    eventQueue

mkDb = do
    time <- getCurrentTime
    return [
        User{
            id = 0,
            credentials = UserCredentials {
                login = "Albert",
                password = "01234567"
            },
            last_seen = time,
            account_created = time
        },
        User{
            id = 1,
            credentials = UserCredentials {
                login = "Boris",
                password = "01234567"
            },
            last_seen = time,
            account_created = time
        }]