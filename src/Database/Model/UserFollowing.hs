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
module Database.Model.UserFollowing where

import           Database.Model.User
import           Import.NoFoundation
import           Instances           ()

share [ mkPersist sqlSettings ] [persistLowerCase|
UserFollowing
  userId                UserId
  followingUserId       UserId
  createdAt             UTCTime
  UniqueFollowingUserId followingUserId
  deriving Show
|]
