{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module App where

-- import Control.Monad.Except (MonadError)
-- import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader
    ( ReaderT
    -- , MonadIO
    -- , MonadReader
    -- , asks
    -- , liftIO
    )
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple
    ( Connection
    , ConnectInfo(..)
    , connect
    , close
    )
import Servant
    ( Handler(..)
    -- , ServantErr
    )

import Config
    ( DBConfig(..)
    , Environment
    , PoolConfig(..)
    )

type AppM = ReaderT Dependencies Handler

data Dependencies = Dependencies
    { getPool :: ConnectionPool
    , getEnvironment :: Environment
    }

type ConnectionPool = Pool Connection

makePool :: DBConfig -> PoolConfig -> IO ConnectionPool
makePool DBConfig{..} PoolConfig{..} =
    createPool (connect ConnectInfo{..}) close stripes connectionTimeout connectionsPerStripe
