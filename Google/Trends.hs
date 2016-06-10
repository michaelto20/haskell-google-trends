{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Google.Trends(queryTrendsWithLogin) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Lens
import HTTP.ThirdParty.FakeUserAgent
import Network.Wreq
import Text.HTML.TagSoup
import qualified Network.Wreq.Session as Ss
import Text.Regex.Posix
import Text.Regex.Base

urlServiceLoginBoxAuth   :: String
urlServiceLoginBoxAuth   = "https://accounts.google.com/ServiceLoginBoxAuth"
urlServiceLoginBoxAuthBS :: BS.ByteString
urlServiceLoginBoxAuthBS = "https://accounts.google.com/ServiceLoginBoxAuth"
urlTrends                :: String
urlTrends                = "http://www.google.com/trends"
urlTrendsComponent       :: String
urlTrendsComponent       = "http://www.google.com/trends/fetchComponent"
urlTrendsReport          :: String
urlTrendsReport          = "http://www.google.com/trends/trendsReport"
urlCookieCheck           :: String
urlCookieCheck           = "https://www.google.com/accounts/CheckCookie"
urlGoogle                :: String
urlGoogle                = "http://www.google.com"

defaultUserAgent = do
    bs <- getLatestBrowserString "Firefox"
    return (BS.pack bs)

toStrictString = BS.concat . LBS.toChunks

{- Google Login -}
loginHeaders options ua = options & 
{-    (proxy ?~ httpProxy "localhost" 8080) &-}
    (header "Accept" .~ ["text/plain"]) &
    (header "User-Agent" .~ [ua]) &
    (header "Content-Type" .~ ["application/x-www-form-urlencoded"]) &
    (header "Referrer" .~ [urlServiceLoginBoxAuthBS])

findLoginInputs tags = 
    [toPair tag | tag <- tags, tag ~== TagOpen ("input" :: String) []]
    where toPair tag = (toStrictString (fromAttrib "name" tag), fromAttrib "value" tag)

getLoginInputs headers session = do
    resp <- Ss.getWith headers session urlServiceLoginBoxAuth
    let body = resp ^. responseBody
    return $ (findLoginInputs . parseTags) body

makeLoginForm form [] email pass = (("Email" := email):("Passwd" := pass):form)
makeLoginForm form ((key, value):inputs) email pass =
    makeLoginForm newForm inputs email pass
    where newForm = ((key := value):form)

doLogin :: String -> String -> (Options -> Ss.Session -> IO a) -> (Options -> Ss.Session -> IO a) -> IO a
doLogin email pass continue fail = do
    Ss.withSession $ \session -> do
        userAgent <- defaultUserAgent
        let headers = loginHeaders defaults userAgent
        inputs <- getLoginInputs headers session
        let form = makeLoginForm [] inputs email pass
        resp <- Ss.postWith headers session urlServiceLoginBoxAuth form
        let cookies = (resp ^? responseCookie "SID", resp ^? responseCookie "HSID")
        case cookies of
            (Nothing, _) -> fail headers session
            (_, Nothing) -> fail headers session
            _            -> continue headers session

{- Google Trends -}
queryParams options keywords = options &
    (param "q" .~ [keywords]) &
    (param "hl" .~ ["en-US"]) &
    (param "cid" .~ ["TIMESERIES_GRAPH_0"]) &
    (param "export" .~ ["5"])

queryTrends keywords headers session = do
    let options = queryParams headers (T.pack keywords)
    resp <- Ss.getWith options session urlTrendsComponent
    let body = resp ^. responseBody
    let parsed = T.unpack (TE.decodeUtf8 (toStrictString body))
    return $ [processPoint point | point <- parseTrends parsed]

parseTrends inp =
    let points = getAllTextMatches (inp =~ pat :: AllTextMatches [] String) in
        processTrends points
    where pat = "([0-9]{4}, [0-9]+, [0-9]+, [0-9]+, [0-9]+[^}]+\\},[^,]+,[^,]+,[0-9]+,)" :: String

processTrends [] = []
processTrends (point:points) =
    ((drop 1 dates) ++ (drop 1 counts)):(processTrends points)
    where dates = getAllTextSubmatches (point =~ datePat :: AllTextSubmatches [] String)
          datePat = "([0-9]{4}), ([0-9]+), [0-9]+," :: String
          counts = getAllTextSubmatches (point =~ countPat :: AllTextSubmatches [] String)
          countPat = "\\},[^,]+,[^,]+,([0-9]+)," :: String

monthName :: String -> String
monthName "0" = "January"
monthName "1" = "February"
monthName "2" = "March"
monthName "3" = "April"
monthName "4" = "May"
monthName "5" = "June"
monthName "6" = "July"
monthName "7" = "August"
monthName "8" = "September"
monthName "9" = "October"
monthName "10" = "November"
monthName "11" = "December"

processPoint :: [String] -> (Integer, String, Integer)
processPoint (year:month:count:[]) = (read year :: Integer, monthName month, read count :: Integer)

queryTrendsWithLogin :: String -> String -> String -> IO (Maybe [(Integer, String, Integer)])
queryTrendsWithLogin email pass keywords = do
    doLogin email pass continue fail
    where continue h s = do 
              results <- queryTrends keywords h s
              return $ Just results
          fail _ _ = return Nothing
          
