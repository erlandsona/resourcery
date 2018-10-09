{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
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
import Servant
import Servant.API.Generic
import Servant.Server.Generic



-- Minor Imports only importing certain functions
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
-- import Database.Beam.Postgres
import Network.Wai (Middleware)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)


-- Local Imports
import Config
import Models (Account)

-- Database Schema

-- API Declaration
data Routes route = Routes
    { _getAccounts :: route :- Get '[JSON] [Account]
    -- , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
    }
  deriving (Generic)

routes :: DBConnection a -> Routes AsServer
routes runDB = Routes
    { _getAccounts = getAccounts runDB
    }


proxy :: Proxy (ToServantApi Routes)
proxy = genericApi (Proxy :: Proxy Routes)

-- getLink :: Int -> Link
-- getLink = fieldLink _get

-- routesLinks :: Routes (AsLink Link)
-- routesLinks = allFieldLinks


app :: DBConnection a -> Application
app = genericServe . routes

logger :: Environment -> Middleware
logger = \case
    Localhost -> logStdoutDev
    Production -> logStdout

type Middlewares = (Wai.Application -> Wai.Application)
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
    ServerConfig {..} <- loadConfig
    pool <- makePool dbConfig poolConfig
    let db = runDB env pool
    let logging = logger env
        stack
            = logging
            . middleware
            . app
            $ db

    run port stack

runDB :: ConnectionPool -> Environment -> DBConnection a
runDB pool = \case
    Localhost -> runBeamPostgresDebug putStrLn pool
    Production -> runBeamPostgres pool

data Dependencies = Dependencies
    { env :: Environment
    , pool :: ConnectionPool
    }
