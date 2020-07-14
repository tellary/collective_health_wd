{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad.IO.Class
import GHC.Stack
import Control.Monad
import Control.Monad.Catch
import Data.Text (Text)
import Test.WebDriver
import Test.WebDriver.Commands.Wait

remoteConfig = useBrowser chrome defaultConfig { wdHost = "host.docker.internal"
                                               , wdPort = 4444
                                               }
wd :: HasCallStack => Text -> WD [Text]
wd pwdIn = do
  openPage "https://my.collectivehealth.com/login"
  close <- waitUntil 10 $ findElem (ByCSS "dls-icon-close")
  click close
  email <- findElem (ById "login-email")
  sendKeys "tellary@gmail.com" email
  pwd   <- findElem (ById "login-password")
  sendKeys pwdIn pwd
  click =<< findElem (ByCSS ".Button")
  activity <- waitUntil 10
              $ findElem (ByXPath "//a[contains(text(),'Activity')]")
  click activity
  loadAllClaims
  waitClaimLoaded
  processClaims $ mapM claimDescription

processClaims f = handle (\(_::SomeException) -> processClaims f) $ do
  claims <- findElems (ByCSS ".Claim")
  liftIO (putStrLn $ "Claims: " ++ (show $ length claims))
  f claims

timeout = 5

timeoutInt :: Int
timeoutInt = floor timeout

waitClaimLoaded = waitUntil timeout $ findElem (ByCSS ".Claim")

loadAllClaims :: WD ()
loadAllClaims = handle (\(_::SomeException) -> return ()) $ do
  more <- waitUntil timeout $ findElem (ByCSS ".Timeline__loadMore")
  click more

claimDescription :: HasCallStack => Element -> WD Text
claimDescription claim = do
  claim <- findElemFrom claim $ ByCSS ".Claim__description"
  getText claim

-- runSession remoteConfig (wd "pwd")
