{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Text (Text)
import Database.Beam
import Servant (Handler)

import Config (DBConnection)

data AccountT f
    = Account
    { accountId :: Columnar f Int
    , accountName :: Columnar f Text
    , accountEmail :: Columnar f Text
    , accountPassword :: Columnar f Text
    } deriving (Generic, Beamable)
deriving instance ToJSON Account
deriving instance Show Account
deriving instance Eq Account

type Account = AccountT Identity
type AccountId = PrimaryKey AccountT Identity
deriving instance ToJSON AccountId
deriving instance Show AccountId
deriving instance Eq AccountId

instance Table AccountT where
    data PrimaryKey AccountT f
        = AccountId (Columnar f Int)
        deriving (Generic, Beamable)
    primaryKey = AccountId . accountId

data ResourceryDb entity
    = ResourceryDb
    { dbAccounts :: entity (TableEntity AccountT)
    } deriving Generic

instance Database be ResourceryDb

resourceryDb :: DatabaseSettings be ResourceryDb
resourceryDb = defaultDbSettings



getAccounts :: DBConnection [Account] -> Handler [Account]
getAccounts db = db $ do
    users <- runSelectReturningList $ select . all_ $ dbAccounts resourceryDb
    return users
