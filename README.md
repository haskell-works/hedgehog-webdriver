# Selenium WebDriver bindings for Hedgehog

This library uses [WebDriver](http://hackage.haskell.org/package/webdriver) as a transport library and contains bindings that are usefil for testing web sites with [Hedgehog](http://hackage.haskell.org/package/hedgehog).

## Examples
These examples can be found in the [Example](example/Main.hs) project.

### Simple test example

A simple test that searches for "Haskell Hedgehog" on GitHub and asserts the results can be expressed as:

```haskell
import Hedgehog
import Hedgehog.WebDriver (withBrowser)

import Hedgehog.WebDriver.Commands
import Test.WebDriver.Commands

import qualified Test.WebDriver             as Web
import qualified Test.WebDriver.Common.Keys as Keys

chromeConfig :: Web.WDConfig
chromeConfig = Web.useBrowser Web.chrome (Web.defaultConfig { Web.wdPort = 9515, Web.wdBasePath = "" })

searchHedgehog :: Property
searchHedgehog = withTests 5 . property $
  withBrowser chromeConfig $ do
    openPage "https://www.github.com/"
    input <- awaitElem (ByName "q2")
    sendKeys "Haskell Hedgehog" input
    sendKeys Keys.enter input

    firstRepo <- awaitElem (ByCSS ".repo-list .repo-list-item h3")
    repoText <- getText firstRepo

    repoText === "hedgehogqa/haskell-hedgehog"

```

Here the same test will be repeated 5 times (hence `withTests 5`), and each time it will be using a new browser window (`withSession` manages that).

### State machine testing

Commands can be defined with `MonadWebTest` constraint:

```haskell
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
```

and then these commands can be used in tests:

```haskell
testGithub :: Property
testGithub = withTests 5 . property $ do
  let initialModel = Model
  let commands = [searchGithub]

  actions <- forAll $ Gen.sequential (Range.linear 1 3) initialModel commands

  withBrowser chromeConfig $ do
    openPage "http://github.com/"
    executeSequential initialModel actions
```

**Note**: Running this very test can accidentally trigger "GutHub Abuse Detection System". It can easily be avoided, for example by detecting when it happens and just refreshing the page, but it is not the point of this example.
