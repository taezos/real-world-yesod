{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ProfileSpec where

-- real-world-yesod
import           Api.Guest
import           Api.Model.Guest
import           Database.Model.Guest
import           Handler.Internal.Email
import           Handler.Internal.Password
import           TestImport

-- persistent
import           Database.Persist

-- uuid
import qualified Data.UUID                 as UUID

-- text
import qualified Data.Text                 as T

spec :: Spec
spec = do
  let guestPasswordTxt = "password"
  let guestUsernameTxt = "guest"
  let guestEmailTxt = "guest@test.com"
  let guestBioTxt = "I am a guest"

  describe "ProfileSpec" $ do
    it "will build a guest profile from a guest entity" $ do
      maybePassword <- mkPassword guestPasswordTxt
      let txtUUID = "123e4567-e89b-12d3-a456-426614174000"
      let guestEntity = do
            password <- maybePassword
            guestKey <- UUID.fromText txtUUID
            pure $ Entity ( GuestKey guestKey ) $ Guest
              { guestFirstName = Nothing
              , guestLastName = Nothing
              , guestEmail = mkEmail guestEmailTxt
              , guestPassword = password
              , guestCreatedAt = currentTestTime
              , guestUsername = guestUsernameTxt
              , guestBio = Just guestBioTxt
              , guestImageLink = Nothing
              }
      mGuestProfile <- toGuestProfile guestEntity
      case mGuestProfile of
        Nothing -> error "no guest profile found"
        Just guestProfile -> do
          shouldBe ( UUID.toText $ guestProfileGuestId guestProfile ) txtUUID

    it "will create a guest record from CreateGuest input" $ do
      let cGuest = CreateGuest
            { createGuestFirstName = Nothing
            , createGuestLastName  = Nothing
            , createGuestEmail     = Just guestEmailTxt
            , createGuestUsername  = guestUsernameTxt
            , createGuestPassword  = guestPasswordTxt
            , createGuestBio       = Just guestBioTxt
            , createGuestImageLink = Nothing
            }
      password <- mkPassword guestPasswordTxt
      let guestRecordRes = toGuestRecord cGuest password currentTestTime
      case guestRecordRes of
        Left err -> error $ T.unpack err
        Right Guest{..} -> do
          shouldBe ( emailToText <$> guestEmail ) ( Just "guest@test.com" )
          shouldBe guestUsername guestUsernameTxt
