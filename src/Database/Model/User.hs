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

-- "Guest" is chosen as a table name because "user" is a reserved name in
-- postgresql. It is them mapped to the user model here.
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
  updatedAt UTCTime
  UniqueUser username
  UniqueEmail email
  deriving Show
|]
