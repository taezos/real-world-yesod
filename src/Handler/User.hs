{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Handler.User where

-- real-world-yesod
import           Api.Model.User
import           Api.User
import           Import

postUserRegisterR :: Handler Value
postUserRegisterR = do
  createUser <- requireCheckJsonBody @Handler @( UserWrapper CreateUser )
  currentTime <- liftIO getCurrentTime
  res <- runDB $ insertUserIO ( userWrapperUser createUser ) currentTime
  case res of
    Left errMsg   -> sendResponseStatus status404 errMsg
    Right userKey -> pure $ toJSON userKey

postUserLoginR :: Handler Value
postUserLoginR = do
  userLogin  <- requireCheckJsonBody @Handler @( UserWrapper UserLogin )
  res <- runDB $ selectUserLoginIO userIdToToken ( userWrapperUser userLogin )
  case res of
    Left errMsg    -> sendResponseStatus status404 errMsg
    Right userAuth -> pure $ toJSON $ UserWrapper userAuth

getCurrentUserR :: Handler Value
getCurrentUserR = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing     -> notAuthenticated
    Just userId -> toJSON . UserWrapper <$> ( runDB $ selectUserByIdIO userId )
