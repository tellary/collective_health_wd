{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- This pragma to make `stylish-haskell` happy
{-# LANGUAGE FlexibleContexts    #-}

import Control.Monad.Catch          (SomeException, handle)
import Control.Monad.IO.Class       (liftIO)
import Data.List                    (groupBy, isInfixOf)
import Data.List.Split              (splitOn)
import Data.Text                    (Text, unpack)
import GHC.Stack                    (HasCallStack)
import Test.WebDriver               (Element, Selector (ByCSS, ById, ByXPath),
                                     WD, WDConfig (..), chrome, click,
                                     defaultConfig, findElem, findElemFrom,
                                     findElems, findElemsFrom, getText,
                                     openPage, runSession, sendKeys, useBrowser)
import Test.WebDriver.Commands.Wait (waitUntil)
import Text.Printf                  (printf)

remoteConfig = useBrowser chrome defaultConfig { wdHost = "host.docker.internal"
                                               , wdPort = 4444
                                               }

wd :: HasCallStack => Text -> Text -> WD [PureClaim]
wd user pwdIn = do
  openPage "https://my.collectivehealth.com/login"
  close <- waitUntil 10 $ findElem (ByCSS "dls-icon-close")
  click close
  email <- findElem (ById "login-email")
  sendKeys user email
  pwd   <- findElem (ById "login-password")
  sendKeys pwdIn pwd
  click =<< findElem (ByCSS ".Button")
  activity <- waitUntil 10
              $ findElem (ByXPath "//a[contains(text(),'Activity')]")
  click activity
  loadAllClaims
  waitClaimLoaded
  mapM pureClaim =<< claims

data Claim
  = Claim
  { claimElement :: Element
  , claimDescriptionPure :: Text
  }

instance Show Claim where
  show (Claim _ t) = show t

claims :: WD [Claim]
claims = handle (\(_::SomeException) -> claims) $ do
  claims <- mapM initClaim =<< findElems (ByCSS ".Claim")
  liftIO (putStrLn $ "Claims: " ++ (show $ length claims))
  return claims

timeout = 5

timeoutInt :: Int
timeoutInt = floor timeout

waitClaimLoaded = waitUntil timeout $ findElem (ByCSS ".Claim")

loadAllClaims :: WD ()
loadAllClaims = handle (\(_::SomeException) -> return ()) $ do
  more <- waitUntil timeout $ findElem (ByCSS ".Timeline__loadMore")
  liftIO $ putStrLn "Found 'load more' button"
  click more
  loadAllClaims

initClaim :: HasCallStack => Element -> WD Claim
initClaim el = do
  claim <- findElemFrom el (ByCSS ".Claim__description")
  desc  <- getText claim
  return $ Claim el desc

claimAmount :: Claim -> WD Double
claimAmount c = do
  e <- findElemFrom (claimElement c) (ByCSS ".Claim__cost")
  t <- drop 1 . unpack <$> getText e
  liftIO (putStrLn $ "Claim amount: " ++ t)
  return . read . concat . splitOn "," $ t

data ClaimStatus = None | Adjusted | Denied deriving (Eq, Show)

claimStatus :: Claim -> WD ClaimStatus
claimStatus c = do
  e <- findElemFrom (claimElement c) (ByCSS ".Claim__status")
  t <- getText e
  case unpack t of
    "ADJUSTED"  -> return Adjusted
    "DENIED"    -> return Denied
    s | (concat . splitOn " " $ s) == "" -> return None
      | otherwise
        -> error $ printf "Unknown claim status found" s

claimDate c = do
  e <- findElemFrom (claimElement c) (ByCSS ".Claim__date")
  getText $ e

data PureClaim
  = PureClaim
  { pureClaimDesc   :: Text
  , pureClaimDate   :: Text
  , pureClaimStatus :: ClaimStatus
  , pureClaimAmount :: Double
  } deriving (Eq, Show)

pureClaim c
  = PureClaim (claimDescriptionPure c)
    <$> claimDate   c
    <*> claimStatus c
    <*> claimAmount c

runClaims user pwd = runSession remoteConfig (wd user pwd)

selectPhysicalTherapyClaims
  = filter (("SELECT PHYSICAL THERAPY" `isInfixOf`) . unpack . pureClaimDesc)

claimDateEq c1 c2
  = pureClaimDate c1 == pureClaimDate c2

-- We currently assume two claims with status
-- `None` and `Adjusted` sit in one day, an error happens otherwise
removeDeniedAndAdjusted
 = concat
   . map removeAdjusted
   . groupBy claimDateEq
   . filter (\c -> pureClaimStatus c /= Denied)
   where removeAdjusted cs
           | hasNoAdjusted = cs
           | length cs == 2 && singleAdjusted = adjustedClaims
           | otherwise = error "Adjusted hack doesn't work, get back to code"
           where
             singleAdjusted = 1 == length adjustedClaims
             adjustedClaims = filter (\c -> pureClaimStatus c == Adjusted) cs
             hasNoAdjusted  = null adjustedClaims

sumSelectPhysicalTherapyAdjustedClaims
  = sum
    . map pureClaimAmount
    . selectPhysicalTherapyClaims
    . removeDeniedAndAdjusted

-- 1. All claims are retrieved,
-- 2. Deleted claims are filtered out,
-- 3. Claims for a certain provider are retained,
-- 4. A one-off hacky solution to filter adjusted claims out is applied,
-- 5. Sum of remaining claims is computed.
runSumSelectPhysicalTherapyAdjustedClaims user pwd
  = sumSelectPhysicalTherapyAdjustedClaims <$> runClaims user pwd

