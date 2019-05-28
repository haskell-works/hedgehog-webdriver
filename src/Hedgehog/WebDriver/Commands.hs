{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Hedgehog.WebDriver.Commands
(
-- * Session
  WebContext(..)
, Millis(..)
, cleanupSession

-- * Browser interaction
-- ** Web navigation
, openPage, openPageWhen
, back, forward, refresh

-- ** Web storage
, Web.WebStorageType(..)
, deleteAllKeys

-- ** Page info
, getCurrentUrl, getSource, getTitle
, screenshot, screenshotBase64, saveScreenshot

-- * Web elements
, Web.Selector(..)
, Web.Element

-- ** Searching for elements
, findElem, findElems, elemMissing
, findElemFrom, findElemsFrom

-- ** Interacting with elements
, getText, getTextWith, clearInput
, assertText, assertTextWith
, setText, sendKeys, sendRawKeys
, click, clickWith, submit
, moveTo

-- ** Element information
, isDisplayed, assertDisplayed, awaitDisplayed
, assertNotDisplayed, awaitNotDisplayed
, isEnabled, assertEnabled, awaitEnabled
, assertDisabled, awaitDisabled
, isSelected, assertSelected, awaitSelected
, tagName, activeElem
, attr, assertAttr

-- ** Mouse gestures
, Web.MouseButton(..)

-- * Lower level functions
, Result(..)
, awaitWD, awaitWDSatisfy
, runWD ,runWDSatisfy
)
where

import Control.Exception           (try)
import Control.Monad               (void)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Reader        (MonadReader, ask)
import Control.Monad.Trans.Control (control, liftBaseWith, restoreM)

import Data.Text              (Text)
import GHC.Generics           (Generic)
import Test.WebDriver.Session (WDSession, getSession)

import Hedgehog.Internal.Source (HasCallStack (..), withFrozenCallStack)

import Hedgehog.WebDriver.Commands.Result
import Hedgehog.WebDriver.Internal.Monad
import Hedgehog.WebDriver.WebContext      (Millis (..), WebContext (..))

import           Test.WebDriver          (Element, FailedCommand (..), Selector (..), WD (..))
import qualified Test.WebDriver          as Web
import qualified Test.WebDriver.Commands as Web

import qualified Control.Retry        as Retry
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text

cleanupSession :: (MonadIO m, MonadReader WebContext m, HasCallStack) => m ()
cleanupSession = withFrozenCallStack $ do
  ctx <- ask
  liftIO $ Web.runWD (session ctx) $ do
    Web.deleteAllKeys Web.LocalStorage
    Web.deleteAllKeys Web.SessionStorage
    Web.deleteVisibleCookies

-- | Navigates to the specified URI
openPage :: (MonadWebTest m, HasCallStack) => Text -> m ()
openPage uri = withFrozenCallStack $ awaitWD (Web.openPage (Text.unpack uri))

-- | Navigates to the specified (new) URI unless the predicate indicates
-- that the current URI is acceptable.
--
-- This is useful when we want to navivate to the website/page unless we are already
-- on this website or page, and we don't care about about the exact match (query string, etc.)
openPageWhen :: (MonadWebTest m, HasCallStack)
  => Text             -- ^ URI to navigate to
  -> (Text -> Bool)   -- ^ The preficate for the current URL. @True@ means "already there".
  -> m Text           -- ^ Depending on the predicate value, either the current URL or the result of a navigation.
openPageWhen uri pred = withFrozenCallStack $ do
  currentUrl <- getCurrentUrl
  if pred currentUrl
    then pure currentUrl
    else openPage uri >> getCurrentUrl

forward :: (MonadWebTest m, HasCallStack) => m ()
forward = withFrozenCallStack $ awaitWD Web.forward

back :: (MonadWebTest m, HasCallStack) => m ()
back = withFrozenCallStack $ awaitWD Web.back

refresh :: (MonadWebTest m, HasCallStack) => m ()
refresh = withFrozenCallStack $ awaitWD Web.back

getSource :: (MonadWebTest m, HasCallStack) => m Text
getSource = withFrozenCallStack $ awaitWD Web.getSource

getTitle :: (MonadWebTest m, HasCallStack) => m Text
getTitle = withFrozenCallStack $ awaitWD Web.getTitle

saveScreenshot :: (MonadIO m, MonadReader WebContext m) => FilePath -> m ()
saveScreenshot path = do
  ctx <- ask
  liftIO $ Web.runWD (session ctx) (Web.saveScreenshot path)

screenshot :: (MonadIO m, MonadReader WebContext m) => m LBS.ByteString
screenshot = do
  ctx <- ask
  liftIO $ Web.runWD (session ctx) Web.screenshot

screenshotBase64 :: (MonadIO m, MonadReader WebContext m) => m LBS.ByteString
screenshotBase64 = do
  ctx <- ask
  liftIO $ Web.runWD (session ctx) Web.screenshotBase64

-- | Returns the current URL
getCurrentUrl :: (MonadWebTest m, HasCallStack) => m Text
getCurrentUrl = withFrozenCallStack $ Text.pack <$> awaitWD Web.getCurrentURL

---------------------------------- HTML5 WEB STORAGE --------------------------

-- |Delete all keys within a given web storage area.
deleteAllKeys :: (MonadIO m, MonadReader WebContext m, HasCallStack) => Web.WebStorageType -> m ()
deleteAllKeys s = withFrozenCallStack $ do
  ctx <- ask
  liftIO $ Web.runWD (session ctx) (Web.deleteAllKeys s)

---------------------------------- FIND ELEMENTS ------------------------------

-- | Find an element on the page using the given element selector.
findElem :: (MonadWebTest m, HasCallStack) => Selector -> m Element
findElem sel = withFrozenCallStack $ awaitWD (Web.findElem sel)

-- | Find all elements on the page matching the given selector.
findElems :: (MonadWebTest m, HasCallStack) => Selector -> m [Element]
findElems sel = withFrozenCallStack $ awaitWD (Web.findElems sel)

-- | Search for an element using the given element as root.
findElemFrom :: (MonadWebTest m, HasCallStack) => Element -> Selector -> m Element
findElemFrom root sel = withFrozenCallStack $ awaitWD (Web.findElemFrom root sel)

-- | Find all elements matching a selector, using the given element as root.
findElemsFrom :: (MonadWebTest m, HasCallStack) => Element -> Selector -> m [Element]
findElemsFrom root sel = withFrozenCallStack $ awaitWD (Web.findElemsFrom root sel)

elemMissing :: (MonadWebTest m, HasCallStack) => Selector -> m Bool
elemMissing sel = do
  ctx <- ask
  liftIO $ null <$> Web.runWD (session ctx) (Web.findElems sel)

-------------------------------- PROPERTIES -----------------------------------

isDisplayed :: (MonadWebTest m, HasCallStack) => Element -> m Bool
isDisplayed elem = withFrozenCallStack $ awaitWD (Web.isDisplayed elem)

assertDisplayed :: (MonadWebTest m, HasCallStack) => Element -> m ()
assertDisplayed elem = void $
  withFrozenCallStack
    $ awaitWDSatisfy id (Web.isDisplayed elem)
    >>= evalResultMsg "Element is expected to be displayed, but wasn't"

awaitDisplayed :: (MonadWebTest m, HasCallStack) => Element -> m Element
awaitDisplayed elem = withFrozenCallStack $ assertDisplayed elem >> pure elem

assertNotDisplayed :: (MonadWebTest m, HasCallStack) => Element -> m ()
assertNotDisplayed elem = void $
  withFrozenCallStack
    $ awaitWDSatisfy not (Web.isDisplayed elem)
    >>= evalResultMsg "Element is expected to be NOT displayed, but it is still there"

awaitNotDisplayed :: (MonadWebTest m, HasCallStack) => Element -> m Element
awaitNotDisplayed elem = withFrozenCallStack $ assertNotDisplayed elem >> pure elem

isEnabled :: (MonadWebTest m, HasCallStack) => Element -> m Bool
isEnabled elem = withFrozenCallStack $ awaitWD (Web.isEnabled elem)

assertEnabled :: (MonadWebTest m, HasCallStack) => Element -> m ()
assertEnabled elem = void $
  withFrozenCallStack
    $ awaitWDSatisfy id (Web.isEnabled elem)
    >>= evalResultMsg "Element is expected to be enabled, but wasn't"

awaitEnabled :: (MonadWebTest m, HasCallStack) => Element -> m Element
awaitEnabled elem = withFrozenCallStack $ assertEnabled elem >> pure elem

assertDisabled :: (MonadWebTest m, HasCallStack) => Element -> m ()
assertDisabled elem = void $
  withFrozenCallStack
    $ awaitWDSatisfy not (Web.isEnabled elem)
    >>= evalResultMsg "Element is expected to be disabled, but it was enabled"

awaitDisabled :: (MonadWebTest m, HasCallStack) => Element -> m Element
awaitDisabled elem = withFrozenCallStack $ assertDisabled elem >> pure elem

isSelected :: (MonadWebTest m, HasCallStack) => Element -> m Bool
isSelected elem = withFrozenCallStack $ awaitWD (Web.isSelected elem)

assertSelected :: (MonadWebTest m, HasCallStack) => Element -> m ()
assertSelected elem = void $
  withFrozenCallStack
    $ awaitWDSatisfy id (Web.isSelected elem)
    >>= evalResultMsg "Element is expected to be selected, but wasn't"

awaitSelected :: (MonadWebTest m, HasCallStack) => Element -> m Element
awaitSelected elem = withFrozenCallStack $ assertSelected elem >> pure elem

tagName :: (MonadIO m, MonadReader WebContext m) => Element -> m Text
tagName elem = do
  ctx <- ask
  liftIO $ Web.runWD (session ctx) (Web.tagName elem)

activeElem :: (MonadWebTest m, HasCallStack) => m Element
activeElem = withFrozenCallStack $ awaitWD Web.activeElem

attr :: (MonadIO m, MonadReader WebContext m) => Element -> Text -> m (Maybe Text)
attr elem attrName = do
  ctx <- ask
  liftIO $ Web.runWD (session ctx) (Web.attr elem attrName)

assertAttr :: (MonadWebTest m, HasCallStack) => Element -> Text -> Maybe Text -> m ()
assertAttr elem attrName expectedValue = withFrozenCallStack $
  void $ awaitWDSatisfy (== expectedValue) (Web.attr elem attrName) >>= evalResult (Just expectedValue)

-- findElemWith :: (MonadWebTest m, HasCallStack)
--   => Selector
--   -> (Element -> WD a)
--   -> (a -> Bool)
--   -> m (Result Element)
-- findElemWith sel proj pred =
--   awaitWD WD a
------------------------------- INTERACTION -----------------------------------

-- | Gets element's text
getText :: (MonadWebTest m, HasCallStack) => Element -> m Text
getText elem = withFrozenCallStack $ awaitWD (Web.getText elem)

-- | Gets element's text when it satisfies the predicate.
-- This will wait retrying until the predicate is satisfied or the timeout is reached.
getTextWith :: (MonadWebTest m, HasCallStack) => Element -> (Text -> Bool) -> m Text
getTextWith elem p = withFrozenCallStack $
  awaitWDSatisfy p (Web.getText elem) >>= evalResult Nothing

-- | Ensures the text of the element is equal to the value provided.
-- This will wait retrying until both values match or the timeout is reached.
assertText :: (MonadWebTest m, HasCallStack) => Element -> Text -> m ()
assertText elem txt = withFrozenCallStack $
  void $ awaitWDSatisfy (== txt) (Web.getText elem) >>= evalResult (Just txt)

-- | Ensures that element's text when it satisfies the predicate.
-- This will wait retrying until the predicate is satisfied or the timeout is reached.
assertTextWith :: (MonadWebTest m, HasCallStack) => Element -> (Text -> Bool) -> m ()
assertTextWith elem p = void $ getTextWith elem p

-- | Sets element's text. The element's value is cleared before the new next is set.
-- This is a combination of 'clearInput' and 'sendKeys'.
setText :: (MonadWebTest m, HasCallStack) => Element -> Text -> m ()
setText elem txt = withFrozenCallStack $ awaitWD (Web.clearInput elem >> Web.sendKeys txt elem)

-- | Send a sequence of keystrokes to an element. All modifier keys are released at the end of the function.
sendKeys :: (MonadWebTest m, HasCallStack) => Element -> Text -> m ()
sendKeys elem txt = withFrozenCallStack $ runWD (Web.sendKeys txt elem)

-- | Similar to sendKeys, but doesn't implicitly release modifier keys afterwards. This allows you to combine modifiers with mouse clicks.
sendRawKeys :: (MonadWebTest m, HasCallStack) => Element -> Text -> m ()
sendRawKeys elem txt = withFrozenCallStack $ runWD (Web.sendKeys txt elem)

clearInput :: (MonadWebTest m, HasCallStack) => Element -> m ()
clearInput elem = withFrozenCallStack $ awaitWD (Web.clearInput elem)

-- | Click on an element.
click :: (MonadWebTest m, HasCallStack) => Element -> m ()
click elem = withFrozenCallStack $ awaitWD (Web.click elem)

-- |Click at the current mouse position with the given mouse button.
clickWith :: (MonadWebTest m, HasCallStack) => Web.MouseButton -> m ()
clickWith btn = withFrozenCallStack $ awaitWD (Web.clickWith btn)

-- | Submit a form element. This may be applied to descendents of a form element as well.
submit :: (MonadWebTest m, HasCallStack) => Element -> m ()
submit elem = withFrozenCallStack $ runWD (Web.submit elem)

-- | Moves the mouse to the given position relative to the active element.
moveTo :: (MonadWebTest m, HasCallStack) => (Int, Int) -> m ()
moveTo pos = withFrozenCallStack $ runWD (Web.moveTo pos)

----------------------------- LOW LEVEL FUNCTIONS --------------------------------
-- | Executes a 'WD' computation within the 'MonadTest' monad, using the given 'WebContext' as state for WebDriver requests.
runWD :: (Show a, MonadWebTest m, HasCallStack) => WD a -> m a
runWD f = withFrozenCallStack $ runWDSatisfy (const True) f >>= evalResult Nothing
{-# INLINE runWD #-}

-- | Runs a "native" 'WD' action until it succeeds or until timeout is reached.
awaitWD :: (Show a, MonadWebTest m, HasCallStack) => WD a -> m a
awaitWD f = withFrozenCallStack $ awaitWDSatisfy (const True) f >>= evalResult Nothing
{-# INLINE awaitWD #-}

-- | Runs a "native" 'WD' action until it succeeds and the result satisfies the predicate,
-- or until timeout is reached.
awaitWDSatisfy :: (MonadIO m, MonadReader WebContext m) => (a -> Bool) -> WD a -> m (Result a)
awaitWDSatisfy pred f = do
  ctx <- ask
  retrying (timeout ctx) (runWDSatisfy' ctx pred f)
{-# INLINE awaitWDSatisfy #-}

runWDSatisfy :: (MonadIO m, MonadReader WebContext m)
  => (a -> Bool)
  -> WD a
  -> m (Result a)
runWDSatisfy pred f = ask >>= (\ctx -> runWDSatisfy' ctx pred f)
{-# INLINE runWDSatisfy #-}

----------------------------- HELPER FUNCTIONS --------------------------------
-- TODO: 'Test.WebDriver.Commands.Wait' but this way seems to allow better
--        error handling / reporting.
-- Play with 'Test.WebDriver.Commands.Wait' more anyway?

-- xxx :: (MonadWebTest m) => m () -> m ()
-- xxx = do
--   pure $ Web.openPage "foo"

retrying :: MonadIO m
  => Millis             -- ^ Keep trying for that long
  -> m (Result a)       -- ^ Action that returns value
  -> m (Result a)       -- ^ Result
retrying (Millis ms) ma = do
  let policy = Retry.limitRetriesByCumulativeDelay (ms * 1000) $ Retry.constantDelay 50000
  Retry.retrying policy (\_ -> pure . not . successResult) (const ma)
{-# INLINE retrying #-}

-- | Executes a 'WD' computation within the 'MonadTest' monad, using the given 'WebContext' as state for WebDriver requests.
-- Uses predicate to filter out unexpected values.
runWDSatisfy' :: MonadIO m
  => WebContext
  -> (a -> Bool)
  -> WD a
  -> m (Result a)
runWDSatisfy' ctx p f = liftIO $ do
  res <- try (Web.runWD (session ctx) f)
  pure $ case res of
          Left err  -> Exception err
          Right val -> if p val then Value val else UnexpectedValue val
{-# INLINE runWDSatisfy' #-}

