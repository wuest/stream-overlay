{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Monad          ( unless, when )
import Control.Exception      ( try )
import Data.Time              ( UTCTime, getCurrentTime )
import Web.ExtraLife.API      ( recentDonations )
import Web.ExtraLife.Donation ( Donation, createdDateUTC, message, displayName, amount)
import Prelude

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.STM         as STM
import qualified Data.Text                      as Text
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Numeric                        as Num   ( showFFloat )
import qualified Network.HTTP.Client            as HTTP  ( HttpException )
import qualified System.Directory               as Dir

import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Middleware.Static  as Static
import qualified Network.WebSockets             as WS

import Opts
import qualified Web
import qualified Server.Const                   as Const
import qualified Twitch.Bot                     as TwitchBot

-- ExtraLife donation tracking

watchDonations :: Bool -> Int -> STM.TChan Donation -> UTCTime -> IO ()
watchDonations verbose elid broadcast prev = do
    Concurrent.threadDelay $ seconds 10
    edonations <- try $ recentDonations elid
    case edonations of
        Left e -> return (e :: HTTP.HttpException) >> watchDonations verbose elid broadcast prev
        Right donations -> do
            let donation = nextDonation prev donations
                next = nextTimestamp prev donation
            alertDonation verbose broadcast donation
            watchDonations verbose elid broadcast next
  where
    seconds :: Int -> Int
    seconds = (*1000000)

nextTimestamp :: UTCTime -> Maybe Donation -> UTCTime
nextTimestamp x Nothing = x
nextTimestamp x (Just d) = if x > y then x else y
  where y = createdDateUTC d

nextDonation :: UTCTime -> Maybe [Donation] -> Maybe Donation
nextDonation _ Nothing = Nothing
nextDonation _ (Just []) = Nothing
nextDonation prev (Just (d:ds)) =
    if createdDateUTC d > prev
        then Just d
        else nextDonation prev $ Just ds

alertDonation :: Bool -> STM.TChan Donation -> Maybe Donation -> IO ()
alertDonation _ _ Nothing = return ()
alertDonation verbose chan (Just donation) = do
    STM.atomically $ STM.writeTChan chan donation
    when verbose $ putStrLn $
        "Donation: " ++ name ++ " (" ++ donated ++ ") " ++ msg
            
  where
    name = case displayName donation of
        Nothing -> "Anonymous"
        Just n  -> Text.unpack n
    msg = case message donation of
        Nothing -> ""
        Just m  -> Text.unpack m
    donated = ("$"++) $ Num.showFFloat (Just 2) (amount donation) ""

-- First run setup

populateDir :: FilePath -> IO ()
populateDir path = do
    BS.writeFile (path ++ "/index.html") $ LBS.toStrict Const.overlayApp
    BS.writeFile (path ++ "/overlay.css") $ LBS.toStrict Const.overlayStyle
    BS.writeFile (path ++ "/overlay.svg") $ LBS.toStrict Const.overlayMask

setupDataDir :: FilePath -> IO ()
setupDataDir path = do
    putStrLn $ "Data directory (" ++ path ++ ") doesn't exist - creating..."
    Dir.createDirectory path
    Dir.createDirectory dest
    populateDir dest
  where
    dest = path ++ "/overlay"

combinedConfig :: Options -> IO Options
combinedConfig options = do
    exists <- path >>= Dir.doesDirectoryExist
    unless exists $ path >>= setupDataDir
    return options
  where
    path = optConfig options


main :: IO ()
main = do
    options <- getOpts >>= combinedConfig
    state <- Concurrent.newMVar []
    base <- optConfig options
    time <- getCurrentTime
    chan <- STM.atomically STM.newBroadcastTChan

    when (ircEnabled options) $ do
        irc <- TwitchBot.initialize (ircNick options) (ircPass options) (ircChannel options)
        _ <- Concurrent.forkIO $ TwitchBot.listen irc
        _ <- Concurrent.forkIO $ TwitchBot.broadcast irc chan
        return ()

    when (extralifeID options > negate 1) $ do
        _ <- Concurrent.forkIO $ watchDonations (optVerbose options) (extralifeID options) chan time
        return ()

    _ <- Concurrent.forkIO $ Web.broadcast state chan

    let settings = Warp.setPort (webPort options) $ Warp.setHost "127.0.0.1" Warp.defaultSettings
    Warp.runSettings settings $ WS.websocketsOr
        WS.defaultConnectionOptions (Web.server state) $
            Static.staticPolicy (Static.addBase $ base ++ "/overlay") $
                Web.static (webPort options)
