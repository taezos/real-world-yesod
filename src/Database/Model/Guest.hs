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
module Database.Model.Guest where

-- real-world-yesod
import           Data.Text
import           Handler.Internal.Email
import           Handler.Internal.Password
import           Import.NoFoundation
import           Instances                 ()

-- uuid
import           Data.UUID

share [ mkPersist sqlSettings ] [persistLowerCase|
Guest
  Id UUID default=uuid_generate_v4()
  firstName Text Maybe
  lastName  Text Maybe
  email     Email
  username  Text
  password  Password
  bio       Text Maybe
  imageLink Text Maybe
  createdAt UTCTime
  UniqueGuest username
  UniqueEmail email
  deriving Show
|]
