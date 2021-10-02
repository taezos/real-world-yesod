{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Handler.Profile where

-- real-world-yesod
import           Api.Model.User
import           Api.User
import           Import

getProfileR :: Text -> Handler Value
getProfileR username = do
  mProfile <- runDB $ selectUserProfileByUsernameIO username
  pure $ toJSON $ ProfileWrapper mProfile

postFollowUserR :: Text -> Handler Value
postFollowUserR username = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      createdAt <- liftIO getCurrentTime
      res <- runDB $ insertUserFollowing userId username createdAt
      case res of
        Left errMsg -> sendResponseStatus status422 errMsg
        Right userFollowingProfile ->
          pure $ toJSON $ ProfileWrapper userFollowingProfile

deleteFollowUserR :: Text -> Handler Value
deleteFollowUserR username = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      deleteRes <- runDB $ removeUserFollowing userId username
      case deleteRes of
        Left errMsg -> sendResponseStatus status422 errMsg
        Right deletedProfile ->
          pure $ toJSON $ ProfileWrapper deletedProfile
