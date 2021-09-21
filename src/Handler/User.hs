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
  createUser <- requireCheckJsonBody @Handler @CreateUser
  currentTime <- liftIO getCurrentTime
  res <- runDB $ insertUserIO createUser currentTime
  case res of
    Left errMsg    -> sendResponseStatus status404 errMsg
    Right userKey -> pure $ toJSON userKey

postUserLoginR :: Handler Value
postUserLoginR = do
  userLogin <- requireCheckJsonBody @Handler @UserLogin
  res <- runDB $ selectUserLoginIO userIdToToken userLogin
  case res of
    Left errMsg     -> sendResponseStatus status404 errMsg
    Right userAuth -> pure $ toJSON userAuth
