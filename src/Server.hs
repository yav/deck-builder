module Server where

import Data.ByteString(ByteString)
import Network.WebSockets.Snap
import Network.WebSockets
import Snap.Http.Server
import Control.Exception

import Data.Aeson(ToJSON,FromJSON)
import qualified Data.Aeson as JS



main :: IO ()
main = quickHttpServe (runWebSocketsSnap theApp)

theApp :: PendingConnection -> IO ()
theApp pending =
  do print (pendingRequest pending)
     conn <- acceptRequest pending
     js <- receiveJS conn
     print (js :: JS.Value)
     sendJS conn (Nothing :: Maybe Bool)
  `catch` \connEx -> print (connEx :: ConnectionException)

sendJS :: ToJSON a => Connection -> a -> IO ()
sendJS conn a = sendTextData conn (JS.encode a)

receiveJS :: FromJSON a => Connection -> IO a
receiveJS conn =
  do txt <- receiveData conn
     case JS.decode' txt of
       Just a -> pure a
       Nothing -> fail "Didn't understand"


