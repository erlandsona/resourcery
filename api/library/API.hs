{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module API where

-- Major imports exposing everything
import Data.Aeson
import Database.Beam
import Servant


-- Minor Imports only importing certain functions
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Data.Yaml.Config (loadYamlSettings, useEnv)
-- import Database.Beam.Postgres
import Network.Wai (Middleware)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)


-- Local Imports
import App
import Config

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

nt :: Dependencies -> AppM a -> Handler a
nt = flip runReaderT

server :: Dependencies -> Application
server deps =
    serve api $
        hoistServer api (nt deps) (return [] :: ServerT API AppM)
        where
            api = (Proxy :: Proxy API)

-- Main
main :: IO ()
main = do
    ServerConfig {..} <- loadYamlSettings ["config/settings.yaml"] [] useEnv
    pool <- makePool dbConfig poolConfig
    let logger =
            case env of
                Localhost -> logStdoutDev
                Production -> logStdout
        deps = Dependencies
            { getPool = pool
            , getEnvironment = env
            }
        middlewares = compression
                    -- . allowCsrf
                    . corsified
        app = middlewares . server $ deps

    run port . logger $ app

type Middlewares = (Wai.Application -> Wai.Application)

-- | @x-csrf-token@ allowance.
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
-- allowCsrf :: Middleware
-- allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

compression :: Middleware
compression = gzip def

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
