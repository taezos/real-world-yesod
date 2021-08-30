{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Instances                 ()

-- uuid
import           Data.UUID

-- persistent
import           Database.Persist.TH

share [ mkPersist sqlSettings ] [persistLowerCase|
Guest
  Id UUID default=uuid_generate_v4()
  firstName Text Maybe
  lastName  Text Maybe
  email     Email
  password  Password
  UniqueGuest email
|]
