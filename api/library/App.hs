{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module App where

import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple
    ( Connection
    , ConnectInfo(..)
    , connect
    , close
    )
import Servant (Handler(..))

import Config
    ( DBConfig(..)
    , PoolConfig(..)
    )


type AppM = ReaderT Dependencies Handler

data Dependencies = Dependencies
    { getPool :: ConnectionPool
    }

type ConnectionPool = Pool Connection

makePool :: DBConfig -> PoolConfig -> IO ConnectionPool
makePool DBConfig{..} PoolConfig{..} =
    createPool (connect ConnectInfo{..}) close stripes connectionTimeout connectionsPerStripe
