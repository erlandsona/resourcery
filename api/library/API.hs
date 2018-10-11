{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module API where

-- Major imports exposing everything
import Servant
import Servant.API.Generic
import Servant.Server.Generic



-- Minor Imports only importing certain functions
-- import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Logger
    ( -- runNoLoggingT
      runStdoutLoggingT
    )
import qualified Data.ByteString.Char8 as Ch8
import Data.List (intercalate)
-- import Data.Pool (withResource)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Persist.Postgresql
    ( ConnectionString
    , ConnectionPool
    , Entity
    , SqlPersistT
    , createPostgresqlPool
    , entityVal
    , runSqlPool
    )
import Database.Persist.Sql (selectList)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

-- Servant NT stuff
import Control.Exception (try)
import Servant.Utils.Enter ((:~>)(..), enter)


-- Local Imports
import Config
import Models

-- Database Schema

-- API Declaration
data Routes route = Routes
    { _getAccounts :: route :- Get '[JSON] [Account]
    -- , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
    }
  deriving (Generic)

routes :: ConnectionPool -> Routes AsServer
routes pool = Routes
    { _getAccounts = getAccounts db
    }
    where
        db = (flip runSqlPool) pool

type Query a = SqlPersistT IO a -> Handler a


getAccounts :: Query a -> Handler [Entity Account]
getAccounts db = do
    dbAccounts <- db $ selectList [] []
    return $ entityVal <$> dbAccounts


proxy :: Proxy (ToServantApi Routes)
proxy = genericApi (Proxy :: Proxy Routes)

-- getLink :: Int -> Link
-- getLink = fieldLink _get

-- routesLinks :: Routes (AsLink Link)
-- routesLinks = allFieldLinks


app :: ConnectionPool -> Application
app = genericServe . routes


type Middlewares = (Application -> Application)
middleware :: Middlewares
middleware = compression
            -- . allowCsrf
            . corsified

-- | @x-csrf-token@ allowance.
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
-- allowCsrf :: Middleware
-- allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)
    where
        -- | Cors resource policy to be used with 'corsified' middleware.
        --
        -- This policy will set the following:
        --
        -- * RequestHeaders: @Content-Type, Authorization, Origin@
        -- * MethodsAllowed: @OPTIONS, GET, PUT, POST@
        appCorsResourcePolicy :: CorsResourcePolicy
        appCorsResourcePolicy = CorsResourcePolicy
            { corsOrigins        = Nothing
            , corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
            , corsRequestHeaders = ["Authorization", "Content-Type", "Origin"]
            , corsExposedHeaders = Nothing
            , corsMaxAge         = Nothing
            , corsVaryOrigin     = False
            , corsRequireOrigin  = False
            , corsIgnoreFailures = False
            }

compression :: Middleware
compression = gzip def


-- Main
main :: IO ()
main = do
    ServerConfig {..} <- loadConf

    let dbConnStr = makeDbConnStr dbConfig
    let logger = case env of
            Localhost -> logStdoutDev
            Production -> logStdout
    let dbLogger = case env of
            Localhost -> runStdoutLoggingT
            -- Test -> runNoLoggingT
            Production -> runStdoutLoggingT
    let numOfPoolStripes = case env of
            Localhost -> 2
            Production -> 8

    pool <- dbLogger (createPostgresqlPool dbConnStr numOfPoolStripes)
    let stack
            = logger
            . middleware
            . app
            $ pool

    run port stack

loadConf :: IO ServerConfig
loadConf = loadYamlSettings ["config/settings.yaml"] [] useEnv

makeDbConnStr :: DBConfig -> ConnectionString
makeDbConnStr DBConfig{..} = Ch8.pack . intercalate " " $ (<>) <$>
    [ "host=", "dbname=", "user=", "password=", "port="] <*>
    [dbHost, dbName, dbUser, dbPass, (show dbPort)]
