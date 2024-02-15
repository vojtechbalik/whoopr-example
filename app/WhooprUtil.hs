{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module WhooprUtil where

import Whoopr.TaskQueue
import Whoopr.EventSender (Task (..), NotificationInitialization (..), DynEventData (..))
import Protolude
import Data.Aeson (ToJSON)
import Data.Aeson.Types ( object, (.=) )

type FilterDescriptorType = ByteString
type TaskType = Task FilterDescriptorType
type TaskQueueType = Chan TaskType

class HasWebhooks env where
    getTaskQueueProducer :: env -> TaskQueueType

type (MonadWebhooks env m) = (MonadIO m, MonadReader env m, HasWebhooks env)

notify :: (MonadWebhooks env m, ToJSON ed) => FilterDescriptorType -> ed -> m ()
notify fd ed = do
    q <- getTaskQueueProducer <$> ask
    let edWithEventType = object [
                "eventType" .= decodeUtf8 fd,
                "eventData" .= ed
            ]
    let task = NotificationInitialization {
        eventData = DynEventData edWithEventType,
        filterDescriptor = fd
    }
    liftIO $ tqSend q $ NotificationInitializationTask task