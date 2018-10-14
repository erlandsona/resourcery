{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Config where

import Data.Aeson (FromJSON)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)


data DBConfig = DBConfig
    { dbPort :: Int
    , dbHost :: String
    , dbUser :: String
    , dbPass :: String
    , dbName :: String
    }
    deriving (FromJSON, Generic)

data ServerConfig = ServerConfig
    { env :: Environment
    , port :: Int
    , dbConfig :: DBConfig
    }
    deriving (FromJSON, Generic)

data Environment
    = Localhost
    | Production
    deriving (FromJSON, Generic)

-- Utilities

type Seconds = NominalDiffTime
