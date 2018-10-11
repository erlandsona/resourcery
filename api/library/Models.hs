{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Data.Aeson
import Data.Text (Text)
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )
import GHC.Generics (Generic)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account sql=accounts
    name Text
    email Text
    password Text
    deriving Generic
|]

instance ToJSON Account
