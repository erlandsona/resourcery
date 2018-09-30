{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module API where

import Data.Aeson
import Data.Text (Text)
import Database.Beam
import Database.PostgreSQL.Simple
import GHC.Word (Word16)
-- import Database.Beam.Postgres
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Pool
import Data.Yaml.Config (loadYamlSettings, useEnv)

-- Database Schema
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

data DBConfig = DBConfig
    { connectPort     :: Word16
    , connectHost     :: String
    , connectUser     :: String
    , connectPassword :: String
    , connectDatabase :: String
    }
    deriving (Generic, FromJSON)

data ServerConfig = ServerConfig
    { port :: Int
    } deriving (Generic, FromJSON)

data Config = Config
    { serverConfig :: ServerConfig
    , dbConfig :: DBConfig
    } deriving (Generic, FromJSON)

-- Main
main :: IO ()
main = do
    Config { serverConfig = ServerConfig{..}, ..} <- loadYamlSettings ["config/settings.yaml"] [] useEnv
    pool <- makePool dbConfig

    run port app

makePool :: DBConfig -> IO (Pool Connection)
makePool DBConfig{..} =
    let
        connectionTimeout = 60 -- Seconds
        connectionsPerStripe = 10
        stripes = 2
    in
        createPool (connect ConnectInfo{..}) close stripes connectionTimeout connectionsPerStripe
