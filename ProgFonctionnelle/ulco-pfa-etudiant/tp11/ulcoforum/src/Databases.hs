{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Databases where

import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import GHC.Generics (Generic)

data Message = Message 
    { _user :: Text
    , _text :: Text
    , _thread :: Text
    } deriving (Generic, Show)

instance FromRow Message where
    fromRow = Message <$> field <*> field <*> field

dbSelectAllMessages :: Connection -> IO [Message]
dbSelectAllMessages conn =
    query_ conn "SELECT message_user, message_text, thread_name \
    \ FROM t_message \
    \INNER JOIN thread ON message_thread = thread_id;"

data Thread = Thread
    { _id :: Int
    , _name :: Text
    } deriving (Generic, Show)

instance FromRow Thread where
    fromRow = Thread <$> field <*> field
{--dbSelectAllMessagesFromThreadId :: Connection -> Int -> IO [Message]
dbSelectAllMessagesFromThreadId conn i=
    query_ conn "SELECT message_user, message_text \
                \FROM t_message \
                \INNER JOIN thread ON message_thread = thread_id \
                \WHERE message_thread = "
--Ajouter l'id du thread

dbInsertIntoMessage:: Connection -> Text -> Text -> Int -> IO ([Text])
dbInsertIntoMessage conn user message thread_id = execute_ conn "INSERT INTO t_message VALUES (?,?)"
                                                                        (user, message, thread_id)
                                                                        --insérer les bonnes valeurs
                                                                    --}
{--main :: IO ()
main = withConnection "thread.db" dbSelectAllMessages >>= mapM_ print--}