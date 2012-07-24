{-# LANGUAGE TemplateHaskell,
             QuasiQuotes,
             OverloadedStrings,
             TypeFamilies,
             MultiParamTypeClasses,
             GeneralizedNewtypeDeriving,
             FlexibleContexts,
             ScopedTypeVariables
  #-}
module Main where

import           Yesod
import           Yesod.Static
import           Control.Applicative ((<$>),(<*>))
import           Control.Concurrent.Chan (Chan, newChan)
import           Network.Wai.Handler.Warp (runSettings, Settings(..), defaultSettings)
import           Data.Text (Text)
import qualified Data.Text.Lazy                 as TL
import qualified Data.HashMap.Strict            as HM
import           Data.Map (Map)
import qualified Data.Map                       as M
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set                       as S
import qualified Network.WebSockets             as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Data.Aeson.Types               as J
import qualified Data.Aeson                     as J
import           Data.Aeson ((.:))
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector                    as V
import           Control.Monad (forever, forM_, liftM, mzero)
import           Control.Monad.IO.Class
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar

import qualified SeqMap                         as SM

type Document = SM.SeqMap AtomId TextAtom

emptyDoc = SM.singleton (AtomId (Timestamp 0) (ClientId 0)) (TextAtom ' ' (Just (Timestamp 0)))

newtype Timestamp = Timestamp { unTimestamp :: Integer }
    deriving (Eq, Ord, Show, J.ToJSON, J.FromJSON)

newtype ClientId  = ClientId  { unClientId  :: Integer }
    deriving (Eq, Ord, Show, Enum, J.ToJSON, J.FromJSON)

data TextAtom = TextAtom
    { taCh      :: Char
    , taRemoved :: Maybe Timestamp
    }

mkTa :: Char -> TextAtom
mkTa ch = TextAtom ch Nothing

data AtomId    = AtomId
                  { aiTime :: Timestamp
                  , aiClient :: ClientId
                  } deriving (Show, Ord, Eq)

instance J.ToJSON AtomId where
   toJSON (AtomId (Timestamp t) (ClientId c)) = J.toJSON [t,c]

-- atoms encoded in json as [time,clientid]
instance J.FromJSON AtomId where
   parseJSON (J.Array v) | V.length v == 2 = AtomId <$> J.parseJSON (v V.! 0) <*> J.parseJSON (v V.! 1)
   parseJSON _ = mzero

insertAtom :: AtomId -> AtomId -> Char -> Document -> Document
insertAtom after aid ch d = go xs
    where
      xs = drop 1 $ SM.toListFrom after d
      go []     = d SM.|> (aid, mkTa ch)
      go ((y,_):ys) | aid < y   = go xs
                    | otherwise = SM.insertBefore y (aid, mkTa ch) d

removeAtom :: AtomId -> Timestamp -> Document -> Document
removeAtom a time = SM.adjust (\(TextAtom ch _) -> TextAtom ch (Just time)) a

-- timestamp in milliseconds
getTimestamp :: MonadIO m => m Timestamp
getTimestamp = liftIO $ liftM (Timestamp . round . (*1000) . utcTimeToPOSIXSeconds) getCurrentTime

{-
collectGarbage :: Integer -> Document -> Document
collectGarbage time d = SM.removeAll ks d
    where
      ks = filter ((< time) . aiTime) (SM.keys d)
-}

data Client = Client
              { clientConnected :: Timestamp       -- timestamp when client connected
              , clientId        :: ClientId        -- unique id
              , clientChan      :: Chan J.Value    -- post outgoing messages to this chan
              }

data Editor = Editor
              { doc       :: MVar (Document, Map ClientId Client)
              , unique    :: MVar ClientId
              , getStatic :: Static
              }

t :: Text -> Text
t = id

mkClientId :: MonadIO m => Editor -> m ClientId
mkClientId e = liftIO (modifyMVar (unique e) $ \u -> let u' = succ u in return (u',u'))

pushToClients :: MonadIO m => Map ClientId Client -> J.Value -> m ()
pushToClients cs msg = do
  forM_ (M.elems cs) $ \c -> liftIO (writeChan (clientChan c) msg)

applyOps :: MonadIO m => Editor -> [Operation] -> m ()
applyOps e ops = do
  mapM_ (applyOp e) ops
  (_, clients) <- liftIO $ readMVar (doc e)
  pushToClients clients $ J.toJSON (OpBlock ops)

applyOp :: MonadIO m => Editor -> Operation -> m ()
applyOp e (InsertAfter a0 ai ch) = liftIO $ modifyMVar_ (doc e) $ \(d,clients) -> do
                                       let d' = insertAtom a0 ai ch d
                                       return (d',clients)
applyOp e (Remove      a) = liftIO $ modifyMVar_ (doc e) $ \(d,clients) -> do
                                       time <- getTimestamp
                                       let d' = removeAtom a time d
                                       return (d', clients)

data Operation = InsertAfter AtomId AtomId Char
               | Remove AtomId
    deriving (Show, Eq, Ord)

data OpBlock = OpBlock [Operation] deriving (Show, Eq, Ord)

instance J.FromJSON OpBlock where
  parseJSON (J.Object o) = OpBlock <$> o .: "actions"
  parseJSON _            = mzero

instance J.ToJSON OpBlock where
  toJSON (OpBlock ops) = J.object [ "actions" .= ops ]

instance J.FromJSON Operation where
    parseJSON (J.Object o) = do
      a <- o .: "action"
      case () of
        () | a == t"insert" -> InsertAfter <$> o .: "after" <*> o .: "id" <*> o .: "ch"
        () | a == t"remove" -> Remove <$> o .: "atom"
        () | otherwise      -> mzero
    parseJSON _ = mzero

instance J.ToJSON Operation where
  toJSON (InsertAfter after aid ch) = J.object [ "action" .= t"insert", "after" .= after, "id" .= aid, "ch" .= ch ]
  toJSON (Remove aid)               = J.object [ "action" .= t"remove", "id" .= aid]

staticSite = static "static"
$(staticFiles "static")

mkYesod "Editor" [parseRoutes|
/       RootR  GET
/edit   EditR  GET
/static StaticR Static getStatic
|]

instance Yesod Editor

getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet|
                            <h1>websockets demo
                            <ul>
                              <li>
                                <a href=@{EditR}>editor
                         |]


getEditR :: Handler RepHtml
getEditR = defaultLayout $ do
             addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"
             addScript     (StaticR codemirror_lib_codemirror_js)
             addScript     (StaticR codemirror_mode_haskell_haskell_js)
             addScript     (StaticR es6_shim_js)
             addScript     (StaticR document_js)
             addStylesheet (StaticR codemirror_lib_codemirror_css)
             toWidget [lucius|
                         #editor {
                           width: 800px;
                           height: 500px;
                         }
                      |]
             toWidget [julius|
                        function wsLocation() {
                          return window.location.href.replace(/^([a-z]+)\:/i, "ws:");
                        }

                        var ignoreChange = false;
                        function handleChange(editor, change) {
                           if(ignoreChange) { return; }
                           var msgs = doc.replace(change.from, change.to, change.text);
                           if(msgs.length > 0) {
                             sock.send(JSON.stringify({ actions: msgs }));
                           }
                        }

                        function refreshEditor() {
                          ignoreChange = true;
                          // fixme: try to preserve cursor position and selection better if other users insert lines etc
                          var sel = cm.somethingSelected();
                          var selStart;
                          var selEnd;
                          var cursor = cm.getCursor();
                          if(sel) {
                            selStart = cm.getCursor(start);
                            selEnd   = cm.getCursor(end);
                          }
                          cm.setValue(doc.currentVersion());
                          cm.setCursor(cursor);
                          if(sel) {
                            cm.setSelection(selStart,selEnd);
                          }
                          ignoreChange = false;
                        }

                        doc = new Document();

                        cm = CodeMirror.fromTextArea($('#editor')[0],
                           { mode:         'text/x-haskell'
                           , lineWrapping: true
                           , lineNumbers:  true
                           , fixedGutter:  true
                           , onChange:     handleChange
                           }
                        );
                        sock = new WebSocket(wsLocation());
                        sock.onmessage = function(evt) {
                          evtJson(evt,function(msg) {
                            if(msg.time) {
                              updateTimestamp(msg.time);
                            }
                            var refresh = false;
                            var r;
                            if(msg.actions) {
                              for(var i=0;i<msg.actions.length;i++) {
                                var a = msg.actions[i];
                                if(a.action === "doc") {
                                  doc.setDocument(a.doc);
                                  refresh = true;
                                } else if(a.action == "clientid") {
                                  setClientId(a.clientId);
                                } else if(a.action === "insert") {
                                  r = doc.applyOp(a);
                                  if(r) { refresh = true; }
                                } else if(a.action === "remove") {
                                  r = doc.applyOp(a);
                                  if(r) { refresh = true; }
                                }
                              }
                            }
                            if(refresh) {
                              refreshEditor();
                            }
                          });
                        }
                      |]
             [whamlet|
               <h1>editor
               <textarea #editor></textarea>
             |]

main :: IO ()
main = do
    d  <- newMVar (emptyDoc, M.empty)
    u  <- newMVar (ClientId 0)
    st <- staticSite
    let master = Editor d u st
        s      = defaultSettings
                  { settingsPort = 3000
                  , settingsIntercept = WS.intercept (sockets master)
                  }
    runSettings s =<< toWaiApp master

receiveJson :: J.FromJSON a => WS.WebSockets WS.Hybi10 a
receiveJson = do
  bs <- WS.receiveData
  case J.decode' bs of
    Nothing -> sendJson (jsonError "invalid message") >> receiveJson
    Just a  -> return a

sendJson :: J.ToJSON a => a -> WS.WebSockets WS.Hybi10 ()
sendJson j = do
    msg <- mkJsonMsg (J.toJSON j)
    WS.sendBinaryData (J.encode msg)

sendSinkJson :: (MonadIO m, J.ToJSON a) => WS.Sink WS.Hybi10 -> a -> m ()
sendSinkJson sink j = do
  msg <- mkJsonMsg (J.toJSON j)
  liftIO $ WS.sendSink sink $ WS.binaryData (J.encode msg)

-- if we send an object, and it does not have a time field, inject it
mkJsonMsg :: MonadIO m => Value -> m J.Value
mkJsonMsg (J.Object o) | not (HM.member "time" o) = do
  t <- getTimestamp
  return $ J.Object (HM.insert "time" (J.toJSON t) o)
mkJsonMsg x = return x

jsonError :: String -> J.Value
jsonError msg = J.object [ "error" .= msg ]

sockets :: Editor -> WS.Request -> WS.WebSockets WS.Hybi10 ()
sockets y req
  | WS.requestPath req == "/edit"  = accept editSocket
  | otherwise                      = WS.rejectRequest req "Not found"
  where
    accept a = WS.acceptRequest req >> a y

editSocket :: Editor -> WS.WebSockets WS.Hybi10 ()
editSocket e = do
  client <- mkClientId e
  sendInit e client
  sink <- WS.getSink
  ch <- liftIO newChan
  t <- liftIO getTimestamp
  liftIO $ addClient e (Client t client ch) 
  tid <- liftIO . forkIO . forever $ do
    acts <- readChan ch
    sendSinkJson sink acts
  forever $ do 
    (OpBlock acts) <- receiveJson
    liftIO $ applyOps e acts
    liftIO $ mapM print acts 
    return ()
  liftIO $ removeClient e client
  return ()

addClient :: Editor -> Client -> IO ()
addClient e c = modifyMVar_ (doc e) $ \(d, m) -> return (d, M.insert (clientId c) c m)

removeClient :: Editor -> ClientId -> IO ()
removeClient e cid = modifyMVar_ (doc e) $ \(d, m) -> return (d, M.delete cid m)

-- encode the whole document
currentVersion :: Document -> Value
currentVersion d =
    let xs = SM.toList d
        encodeAtom (a,v) = J.toJSON [ J.toJSON (aiTime a)
                                    , J.toJSON (aiClient a)
                                    , J.toJSON (taCh v)
                                    , J.toJSON (isJust $ taRemoved v)
                                    ]
    in  J.toJSON $ map encodeAtom xs

sendInit :: Editor -> ClientId -> WS.WebSockets WS.Hybi10 ()
sendInit e c = do
  d <- liftIO (readMVar $ doc e)
  sendJson $ actions [ J.object [ "action" .= t"clientid", "clientId" .= c]
                     , J.object [ "action" .= t"doc", "doc" .= currentVersion (fst d) ]
                     ]

actions :: [J.Value] -> J.Value
actions as = J.object ["actions" .= as ]

