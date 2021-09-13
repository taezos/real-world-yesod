{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Api.Guest where

-- real-world-yesod
import           Api.Common
import           Api.Model.Guest
import           Database.Model.Guest
import           Handler.Internal.Email
import           Handler.Internal.Password
import           Import.NoFoundation

selectGuestProfileByUsernameIO
  :: MonadIO m
  => Text
  -> SqlPersistT m ( Maybe GuestProfile )
selectGuestProfileByUsernameIO username = do
  mGuestEntity <- selectFirst [ GuestUsername ==. username ] []
  toGuestProfile mGuestEntity

toGuestProfile
  :: Monad m
  => Maybe ( Entity Guest )
  -> m ( Maybe GuestProfile )
toGuestProfile mGuestEntity =
  pure $ mGuestEntity
    <&> \( Entity gKey Guest{..}) -> GuestProfile
      { guestProfileGuestId = unGuestKey gKey
      , guestProfileEmail = emailToText <$> guestEmail
      , guestProfileFirstName = guestFirstName
      , guestProfileLastName = guestLastName
      , guestProfileUsername = guestUsername
      , guestProfileBio = guestBio
      , guestProfileImageLink = guestImageLink
      }

insertGuestIO
  :: MonadIO m
  => CreateGuest
  -> UTCTime
  -> SqlPersistT m ( Either Text DbRecordKey )
insertGuestIO cGuest@CreateGuest{..} createdOn = do
  maybePass <- mkPassword createGuestPassword
  case toGuestRecord cGuest maybePass createdOn of
    Left errMsg -> pure $ Left errMsg
    Right guest -> Right . toRecordId <$> insert guest
  where
    toRecordId :: Key Guest -> DbRecordKey
    toRecordId kGuest = DbRecordKey ( unGuestKey kGuest )

toGuestRecord :: CreateGuest -> Maybe Password -> UTCTime -> Either Text Guest
toGuestRecord CreateGuest{..} maybePass createdAt = do
  case mkCreds maybePass of
    Nothing -> Left "Invalid Email and/or Password"
    Just password ->
      Right $ Guest
        { guestFirstName = Nothing
        , guestLastName = Nothing
        , guestEmail = mkEmail =<< createGuestEmail
        , guestUsername = createGuestUsername
        , guestPassword = password
        , guestCreatedAt = createdAt
        , guestBio = createGuestBio
        , guestImageLink = createGuestImageLink
        }
  where
    mkCreds :: Maybe Password -> Maybe Password
    mkCreds mPass = do
      password <- mPass
      pure password
