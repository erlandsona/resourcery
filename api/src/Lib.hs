{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Lib where

import Data.Aeson
import Data.Text (Text)
import Database.Beam
-- import Database.Beam.Postgres
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- Database Configs
data UserT f
    = User
    { userId        :: Columnar f Int
    , userEmail     :: Columnar f Text
    , userFirstName :: Columnar f Text
    , userLastName  :: Columnar f Text
    , userPassword  :: Columnar f Text
    } deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance ToJSON User
deriving instance Show User
deriving instance Eq User

deriving instance ToJSON UserId
deriving instance Show UserId
deriving instance Eq UserId

instance Table UserT where
    data PrimaryKey UserT f
        = UserId (Columnar f Int)
        deriving (Generic, Beamable)
    primaryKey = UserId . userId


data ResourceryDb entity
    = ResourceryDb
    { user :: entity (TableEntity UserT)
    } deriving Generic

instance Database be ResourceryDb

resourceryDb :: DatabaseSettings be ResourceryDb
resourceryDb = defaultDbSettings


-- API Declaration
type API = "users" :> Get '[JSON] [User]

app :: Application
app = serve api server

-- API implimentation
api :: Proxy API
api = Proxy

server :: Server API
server = return []

-- Main
startApp :: IO ()
startApp = run 8080 app
