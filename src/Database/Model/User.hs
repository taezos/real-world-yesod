{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Database.Model.User where

-- real-world-yesod
import           Data.Text
import           Handler.Internal.Email
import           Handler.Internal.Password
import           Import.NoFoundation
import           Instances                 ()

-- uuid
import           Data.UUID

-- User is chosen as a model name because "user" is a reserved word in psql.
-- User will then be mapped to user(s) later using DTOs in Api.Model.User and
-- Api.User.
-- see https://www.postgresql.org/docs/13/sql-keywords-appendix.html
share [ mkPersist sqlSettings ] [persistLowerCase|
User sql=guest
  Id UUID default=uuid_generate_v4()
  firstName Text Maybe
  lastName  Text Maybe
  email     Email
  username  Text
  password  Password
  bio       Text Maybe
  imageLink Text Maybe
  createdAt UTCTime
  UniqueUser username
  UniqueEmail email
  deriving Show
|]
