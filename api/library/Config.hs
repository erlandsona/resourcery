{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Config where

import Data.Aeson (FromJSON)
import Data.Time.Clock (NominalDiffTime)
import Database.Beam (Generic)
import GHC.Word (Word16)


data DBConfig = DBConfig
    { connectPort :: Word16
    , connectHost :: String
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    }
    deriving (FromJSON, Generic)

data PoolConfig = PoolConfig
    { connectionTimeout :: Seconds
    , connectionsPerStripe :: Int
    , stripes :: Int
    }
    deriving (FromJSON, Generic)

data ServerConfig = ServerConfig
    { env :: Environment
    , port :: Int
    , dbConfig :: DBConfig
    , poolConfig :: PoolConfig
    }
    deriving (FromJSON, Generic)

data Environment
    = Localhost
    | Production
    deriving (FromJSON, Generic)

-- Utilities

type Seconds = NominalDiffTime
