module TestImport
  ( module X
  , currentTestTime
  ) where

-- classy-prelude
import           ClassyPrelude   as X

-- hspec
import           Test.Hspec      as X

-- time
import           Data.Time.Clock

currentTestTime :: UTCTime
currentTestTime = UTCTime ( fromGregorian 2021 9 13 ) ( secondsToDiffTime 0 )
