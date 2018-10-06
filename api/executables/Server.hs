-- Just some pomp and circumstance to make compilers happy
-- Run bin/server to run the server with "Code Reloading!"
module Main (main) where
import Network.Wai.Handler.Warp (run)

import Config (ServerConfig(..), loadConfig)
import API (logger, middleware, app)

-- Main
main :: IO ()
main = do
    ServerConfig env_ port_ _ _ <- loadConfig
    -- pool <- makePool dbConfig poolConfig
    -- let dependencies = Dependencies
    --         { getPool = pool
    --         }
    let logging = logger env_
        stack = logging . middleware $ app

    run port_ stack

