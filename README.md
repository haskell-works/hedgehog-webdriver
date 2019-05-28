# Selenium WebDriver bindings for Hedgehog

This library uses [WebDriver](http://hackage.haskell.org/package/webdriver) as a transport library and contains bindings that are usefil for testing web sites with [Hedgehog](http://hackage.haskell.org/package/hedgehog).

## Examples

### Simple test example

A simple test that searches for "Haskell Hedgehog" on GitHub and asserts the results can be expressed as:

```haskell
import Hedgehog
import Hedgehog.WebDriver (withSession)
import Hedgehog.WebDriver.Commands

import qualified Test.WebDriver as Web
import qualified Test.WebDriver.Common.Keys as Web

chromeConfig :: Web.WDConfig
chromeConfig = Web.useBrowser Web.chrome (Web.defaultConfig { Web.wdPort = 9515, Web.wdBasePath = "" })

searchHedgehog :: Property
searchHedgehog = withTests 5 . property $
  withSession chromeConfig $ do
    openPage "https://www.github.com/"
    input <- findElem (ByName "q")
    setText input "Haskell Hedgehog"
    sendKeys input Web.enter

    firstRepo <- findElem (ByCSS ".repo-list .repo-list-item h3")
    repoText <- getText firstRepo

    repoText === "hedgehogqa/haskell-hedgehog"

```

Here the same test will be repeated 5 times (hence `withTests 5`), and each time it will be using a new browser window (`withSession` manages that).

### State machine testing

Commands can be defined with `MonadWebTest` constraint:

```haskell
data Search (v :: Type -> Type) = Search Text
  deriving Show
instance HTraversable Search where
  htraverse _ (Search txt) = pure (Search txt)

searchRepo :: forall g m. (MonadGen g, MonadWebTest m)
  => Command g m Model
searchRepo = Command gen exec
  [ Ensure $ \_ _ _ txt ->
      assert $ "Haskell" `Text.isInfixOf` txt
  ]
  where
    gen :: Model Symbolic -> Maybe (g (Search Symbolic))
    gen m = Just $ Search <$> Gen.element
              ["Haskell Hedgehog", "Haskell Avro"]

    exec :: Search Concrete -> m Text
    exec (Search txt) = do
      input <- findElem (ByName "q")
      setText input cmd
      sendKeys input Web.enter
      findElem (ByCSS ".repo-list .repo-list-item") >>= getText
```

and then these commands can be used in tests:

```haskell
testGitHub :: Property
testGitHub = withTests 5 . property $ do
  let initialModel = Model Tour
  let commands = [searchRepo]

  actions <- forAll $ Gen.sequential (Range.linear 1 3) initialModel commands

  withSession chromeConfig $ do
    openPage "http://github.com/"
    executeSequential initialModel (traceShowId actions)
```

**Disclaimer**: Running this very test can accidentally trigger "GutHub Abuse Detection System". It can easily be avoided, for example by detecting when it happens and just refreshing the page, but it is not the point of this example.
