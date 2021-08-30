{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "Profile page" $ do
    it "asserts no access to my-account for anonymous users" $ do
      get ProfileR
      statusIs 403
