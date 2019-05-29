{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hedgehog
import Hedgehog.WebDriver (withBrowser)

import qualified Test.WebDriver             as Web
import qualified Test.WebDriver.Common.Keys as Web
import qualified Test.WebDriver.Session     as Web

import Test.WebDriver.Commands

chromeConfig :: Web.WDConfig
chromeConfig = Web.useBrowser Web.chrome (Web.defaultConfig { Web.wdPort = 9515, Web.wdBasePath = "" })

searchHedgehog :: Property
searchHedgehog = withTests 5 . property $
  withBrowser chromeConfig $ do
    sess <- Web.getSession
    openPage "https://www.github.com/"
    input <- findElem (ByName "q")
    sendKeys "Haskell Hedgehog" input
    sendKeys Web.enter input

    firstRepo <- findElem (ByCSS ".repo-list .repo-list-item h3")
    repoText <- getText firstRepo

    repoText === "hedgehogqa/haskell-hedgehog"

main :: IO ()
main = do
  _ <- checkSequential $ Group "Main"
          [ --("Machine prop", testMapPage chromeConfig)
          --,
          -- ("XXX", xxx chromeConfig)
          ("Test GH", searchHedgehog)
          ]
  pure ()
