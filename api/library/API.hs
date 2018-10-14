{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module API where

-- Major imports exposing everything
import Servant
import Servant.API.Generic
import Servant.Server.Generic



-- Minor Imports only importing certain functions
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
-- import Control.Monad.Except (ExceptT(..))
import Control.Monad.Logger
    ( -- runNoLoggingT
      runStdoutLoggingT
    )
import qualified Data.ByteString.Char8 as Char8
-- import Data.Pool (withResource)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Persist.Class
    ( PersistQueryRead
    , PersistRecordBackend
    )
import Database.Persist.Postgresql
    ( ConnectionPool
    , Entity
    , SqlBackend
    , createPostgresqlPool
    , printMigration
    , runMigration
    , runSqlPool
    )
import Database.Persist.Sql (selectList)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)


-- Local Imports
import Config
import Models

-- Database Schema

-- API Declaration
-- EG:
-- data ApiRoutes path = ApiRoutes
--     { allAccounts :: path :- Version :> "accounts" :> Get '[JSON] [Entity Account]
--     , allShows :: path :- Version :> "shows" :> Get '[JSON] [Entity Gig]
--     , createShow :: path :- Version :> "shows" :> ReqBody '[JSON] Gig :> Post '[JSON] (Entity Gig)
--     , deleteShow :: path :- Version :> "shows" :> Capture "id" (Key Gig) :> DeleteNoContent '[JSON] NoContent
--     } deriving Generic
-- type ApiRouter = ToServant (ApiRoutes AsApi)

data Routes path = Routes
    { accounts :: path :- "accounts" :> Get '[JSON] [Entity Account]
    -- , users :: path :- "users" :> Get '[JSON] [Entity User]
    -- , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
    }
  deriving (Generic)

routes :: ConnectionPool -> Routes AsServer
routes pool = Routes
    { accounts = db index
    -- , users = db index
    }
    where
        db :: ReaderT SqlBackend IO a -> Handler a
        db = liftIO . (`runSqlPool` pool)

index ::
    ( MonadIO m
    , PersistQueryRead backend
    , PersistRecordBackend record backend
    ) =>
    ReaderT backend m [Entity record]
index = selectList [] []

app :: ConnectionPool -> Application
app pool = genericServe (routes pool)


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

loadConf :: IO ServerConfig
loadConf = loadYamlSettings ["config/settings.yaml"] [] useEnv
-- Main
main :: IO ()
main = do
    conf@ServerConfig{env, port} <- loadConf
    let logging = case env of
            Localhost -> logStdoutDev
            Production -> logStdout

    pool <- mkPool conf

    flip runSqlPool pool $ do
        printMigration migrateAll
        runMigration migrateAll

    let stack
            = logging
            . middleware
            . app
            $ pool

    putStrLn $ "Serving on PORT: " ++ show port
    run port stack


mkPool :: ServerConfig -> IO ConnectionPool
mkPool ServerConfig
    { dbConfig = DBConfig{..}
    , env
    } =
    runStdoutLoggingT $ createPostgresqlPool connectionString connectionsInPool
    where
        connectionsInPool = case env of
            Localhost -> 2
            Production -> 8
        connectionString = Char8.pack $ unwords
            [ "host="++dbHost
            , "port="++show dbPort
            , "user="++dbUser
            , "password="++dbPass
            , "dbname="++dbName
            ]
