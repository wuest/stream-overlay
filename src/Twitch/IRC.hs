module Twitch.IRC where

import Control.Monad.Reader ( ReaderT, asks, liftIO )
import Control.Monad        ( when )
import Data.List            ( isPrefixOf )
import Network
import System.IO
import Text.Printf

import Prelude

data Bot = Bot { socket  :: Handle
               , nick    :: String
               , channel :: String
               }
type IRCBot = ReaderT Bot IO

server :: String
server = "irc.chat.twitch.tv"

loginSuccess :: String
loginSuccess = ":Welcome, GLHF!"

port :: PortID
port = PortNumber 6667

connect :: String -> String -> IO Bot
connect ircnick chan = do
    h <- connectTo server port
    hSetBuffering h NoBuffering
    return (Bot h ircnick chan)

auth :: String -> IRCBot ()
auth pass = do
    n <- asks nick
    c <- asks channel
    command "PASS" $ "oauth:" ++ pass
    command "NICK" n
    command "JOIN" $ "#" ++ c
    -- TODO: Handle auth failure

chanSay :: String -> IRCBot ()
chanSay msg = do
    chan <- asks channel
    command ("PRIVMSG #"++chan) (":"++msg)

write :: String -> IRCBot ()
write cs = do
    h <- asks socket
    liftIO $ hPrintf h "%s" cs

command :: String -> String -> IRCBot ()
command cmd arg = write $ cmd ++ " " ++ arg ++ "\r\n"

listen :: IRCBot ()
listen = do
    h <- asks socket
    line <- liftIO $ hGetLine h
    when (ping line) $ pong line
    listen
  where
    ping line = "PING" `isPrefixOf` line
    pong line = command "PONG" $ (":"++) $ drop 6 line
