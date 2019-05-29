{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Kind (Type)
import Data.Text (Text)

import Hedgehog
import Hedgehog.WebDriver (MonadWebTest, withBrowser)

import Hedgehog.WebDriver.Commands
import Test.WebDriver.Commands

import qualified Data.Text                  as Text
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import qualified Test.WebDriver             as Web
import qualified Test.WebDriver.Common.Keys as Keys

chromeConfig :: Web.WDConfig
chromeConfig = Web.useBrowser Web.chrome (Web.defaultConfig { Web.wdPort = 9515, Web.wdBasePath = "" })

-------------------------- PROPERTY TEST EXAMPLE ------------------------------

searchHedgehog :: Property
searchHedgehog = withTests 5 . property $
  withBrowser chromeConfig $ do
    openPage "https://www.github.com/"
    input <- evalM $ awaitElem (ByName "q")
    sendKeys "Haskell Hedgehog" input
    sendKeys Keys.enter input

    firstRepo <- awaitElem (ByCSS ".repo-list .repo-list-item h3")
    repoText <- getText firstRepo

    repoText === "hedgehogqa/haskell-hedgehog"

-------------------------- STATE MACHINE EXAMPLE ------------------------------

data Model (v :: Type -> Type) = Model {}

data Search (v :: Type -> Type) = Search Text
  deriving Show
instance HTraversable Search where
  htraverse _ (Search txt) = pure (Search txt)

searchGithub :: forall g m. (MonadGen g, MonadWebTest m)
  => Command g m Model
searchGithub = Command gen exec
  [ Ensure $ \_ _ _ txt ->
      assert $ "Haskell" `Text.isInfixOf` txt
  ]
  where
    gen :: Model Symbolic -> Maybe (g (Search Symbolic))
    gen m = Just $ Search <$> Gen.element
              ["Haskell Hedgehog", "Haskell Avro"]

    exec :: Search Concrete -> m Text
    exec (Search txt) = do
      input <- awaitElem (ByName "q")
      sendKeys txt input
      sendKeys Keys.enter input
      awaitElem (ByCSS ".repo-list .repo-list-item") >>= getText

testGithub :: Property
testGithub = withTests 5 . property $ do
  let initialModel = Model
  let commands = [searchGithub]

  actions <- forAll $ Gen.sequential (Range.linear 1 3) initialModel commands

  withBrowser chromeConfig $ do
    openPage "http://github.com/"
    executeSequential initialModel actions

main :: IO ()
main = do
  _ <- checkSequential $ Group "Main"
          [ ("Simple test", searchHedgehog)
          , ("State machine test", testGithub)
          ]
  pure ()



