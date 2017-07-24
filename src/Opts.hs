{-# LANGUAGE OverloadedStrings #-}

module Opts ( Options
            , getOpts
            , optVerbose, optConfig
            , ircEnabled, ircNick, ircPass, ircChannel
            , webPort
            , extralifeID
            ) where
 
import Prelude
import System.Console.GetOpt
import System.Environment    (getProgName, getArgs)
import System.Exit           (exitSuccess)
import System.IO             (hPutStrLn, stderr)

import qualified System.Directory as Dir

data Options = Options { optVerbose  :: Bool
                       , optConfig   :: IO FilePath
                       -- IRC options
                       , ircEnabled  :: Bool
                       , ircNick     :: String
                       , ircPass     :: String
                       , ircChannel  :: String
                       -- Webserver options
                       , webPort     :: Int
                       -- ExtraLife Options
                       , extralifeID :: Int
                       }

defaultOptions :: Options
defaultOptions = Options { optVerbose  = False
                         , optConfig   = Dir.getXdgDirectory Dir.XdgData "streamoverlay"
                         , ircEnabled  = True
                         , ircNick     = "Nickname"
                         , ircPass     = "oauth_token"
                         , ircChannel  = "stream_channel"
                         , webPort     = 8000
                         , extralifeID = (negate 1)
                         }

version :: String
version = "0.1.0.0"

printVersion :: Options -> IO Options
printVersion _ = do
    hPutStrLn stderr $ "Version " ++ version
    exitSuccess

printHelp :: Options -> IO Options
printHelp _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

configLocation :: FilePath -> Options -> IO Options
configLocation arg opt = return opt { optConfig = Dir.makeAbsolute arg }

verbose :: Options -> IO Options
verbose opt = return opt { optVerbose = True }

disableIRC :: Options -> IO Options
disableIRC opt = return opt { ircEnabled = False }

setNick :: String -> Options -> IO Options
setNick arg opt = return opt { ircNick = arg }

setPass :: String -> Options -> IO Options
setPass arg opt = return opt { ircPass = arg }

setChannel :: String -> Options -> IO Options
setChannel arg opt = return opt { ircChannel = arg }

setExtralifeID :: String -> Options -> IO Options
setExtralifeID arg opt = return opt { extralifeID = read arg :: Int }

setWebPort :: String -> Options -> IO Options
setWebPort arg opt = return opt { webPort = read arg :: Int }

blank :: OptDescr (Options -> IO Options)
blank = Option [] [] (NoArg return) ""

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['c'] ["config"]
        (ReqArg configLocation "CONFIG") "Configuration file"

    , Option ['v'] ["verbose"]
        (NoArg verbose) "Enable verbose messages"

    , Option ['V'] ["version"]
        (NoArg printVersion) "Print version"

    , Option ['h', '?'] ["help"]
        (NoArg printHelp) "Show help"

    , blank

    , Option ['I'] ["disableirc"]
        (NoArg disableIRC) "Disable IRC functionality"

    , Option ['n'] ["nick"]
        (ReqArg setNick "USERNAME") "Twitch username for the IRC bot to use"

    , Option ['t'] ["token"]
        (ReqArg setPass "TOKEN") "Twitch OAuth token (without \"oauth:\")"

    , Option ['C'] ["channel"]
        (ReqArg setChannel "CHANNEL") "Twitch channel to join"

    , blank

    , Option ['p'] ["webport"]
        (ReqArg setWebPort "PORT") "Port for the webserver to run on"

    , blank

    , Option ['e'] ["extralife"]
        (ReqArg setExtralifeID "EXTRALIFE_ID") "ExtraLife account to watch"
    ]

getOpts :: IO Options
getOpts = do
    args <- getArgs
    let (actions, _nonoptions, _errors) = getOpt RequireOrder options args
    foldl (>>=) (return defaultOptions) actions
