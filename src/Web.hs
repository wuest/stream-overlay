{-# LANGUAGE OverloadedStrings #-}

module Web ( server, static, broadcast ) where

import Control.Monad          ( forever )
import Data.Text              ( Text )
import Web.ExtraLife.Donation ( Donation, displayName, amount, message )
import Prelude

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.STM         as STM
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Internal       as BS ( c2w )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy.Encoding        as Text ( decodeUtf8 )
import qualified Data.Text.Lazy                 as Text ( toStrict )

import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.WebSockets             as WS

import qualified Server.Const                   as Const
import qualified Server.Json                    as Json

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

textHtml :: Http.Header
textHtml = (Http.hContentType, "text/html")

textJs :: Http.Header
textJs = (Http.hContentType, "text/javascript")

globalJS :: Int -> LBS.ByteString
globalJS port = LBS.fromStrict . BS.pack $ fmap BS.c2w ("webPort = \"" ++ show port ++ "\";\n")

broadcast :: Concurrent.MVar State -> STM.TChan Donation -> IO loop
broadcast stateRef broadcastChan = do
    chan <- STM.atomically $ STM.dupTChan broadcastChan
    forever $ do
        donation <- STM.atomically $ STM.readTChan chan
        sendFrom (negate 1) stateRef $ constructMessage donation

constructMessage :: Donation -> Text
constructMessage donation =
    Text.toStrict $ Text.decodeUtf8 $ Aeson.encode
        Json.Message { Json.userHandle = name
                       , Json.eventType = "donation"
                       , Json.amount = amount donation
                       , Json.message = msg
                       }
  where
    name = Maybe.fromMaybe "Anonymous" (displayName donation)
    msg = Maybe.fromMaybe "" (message donation)

nextId :: State -> ClientId
nextId = Maybe.maybe 0 (1 +) . maxM . List.map fst

maxM :: Ord a => [a] -> Maybe a
maxM [] = Nothing
maxM xs = Just $ maximum xs

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
    let clientId = nextId state
    return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

sendFrom :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
sendFrom clientId stateRef msg = do
    clients <- Concurrent.readMVar stateRef
    let otherClients = withoutClient clientId clients
    Monad.forM_ otherClients $ \(_, conn) ->
        WS.sendTextData conn msg

startApp :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
startApp conn clientId stateRef = Monad.forever $
    WS.receiveData conn >>= sendFrom clientId stateRef

static :: Int -> Wai.Application
static port request respond = respond $ case Wai.rawPathInfo request of
    "/"           -> Wai.responseLBS Http.status200 [textHtml] Const.overlayApp
    "/global.js"  -> Wai.responseLBS Http.status200 [textJs] $ globalJS port
    "/overlay.js" -> Wai.responseLBS Http.status200 [textJs] Const.overlayJS

    _             -> Wai.responseLBS Http.status404 [textHtml] "Not found"

server :: Concurrent.MVar State -> WS.ServerApp
server stateRef pendingConn = do
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateRef
    WS.forkPingThread conn 30
    Exception.finally
        (startApp conn clientId stateRef)
        (disconnectClient clientId stateRef)
