{-# LANGUAGE OverloadedStrings #-}

module Twitch.Bot ( broadcast, initialize, listen ) where

import Control.Monad          ( forever )
import Control.Monad.Reader   ( runReaderT )
import Web.ExtraLife.Donation ( Donation, donorName, donationAmount )
import Numeric                ( showFFloat )
import Prelude

import qualified Control.Concurrent.STM         as STM
import qualified Data.Text                      as Text

import qualified Twitch.IRC                     as IRC

constructIRCMessage :: Donation -> String
constructIRCMessage donation =
    name ++ " donated " ++ amount ++ " to ExtraLife!"
  where
    name = case donorName donation of
        Nothing -> "Anonymous"
        Just n  -> Text.unpack n
    amount = ("$"++) $ showFFloat (Just 2) (donationAmount donation) ""

broadcast :: IRC.Bot -> STM.TChan Donation -> IO loop
broadcast irc broadcastChan = do
    chan <- STM.atomically $ STM.dupTChan broadcastChan
    forever $ do
        donation <- STM.atomically $ STM.readTChan chan
        runReaderT (IRC.chanSay $ constructIRCMessage donation) irc

listen :: IRC.Bot -> IO ()
listen = runReaderT IRC.listen 

initialize :: String -> String -> String -> IO IRC.Bot
initialize nick pass channel = do
    irc <- IRC.connect nick channel
    runReaderT (IRC.auth pass) irc
    return irc
