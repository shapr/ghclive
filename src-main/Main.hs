{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Main where
import           Control.Monad.Trans
import qualified Data.ByteString                as BS
import           Data.Maybe                     (fromMaybe, isJust, isNothing)
import           Data.Monoid
import qualified Data.Text.Lazy                 as T
import           Language.Haskell.Interpreter   hiding (get)
import           Network.Curl.Download
import           Network.Wai.Handler.Warp       (Settings(..), defaultSettings, runSettings)
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, href, rel, src, type_)
import           Yesod
import           Yesod.Static

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                  (forM_, forever, liftM, mzero, void)
import           Control.Monad.Error.Class
import           Data.Aeson                     ((.:), (.=))
import           Data.Char                      (isUpper)
import qualified Data.Text                      as ST
import           Data.Typeable
import           Network.Web.GHCLive.Display
import           Text.Blaze
import           Text.Blaze.Internal            (preEscapedText, text)
import           Text.Blaze.Renderer.Text       (renderMarkup)

import qualified Data.Aeson                     as J
import qualified Data.Aeson.Types               as J
import qualified Data.HashMap.Strict            as HM
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import qualified Data.Text.IO                   as DTI
import           Data.Time.Clock                (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX          (utcTimeToPOSIXSeconds)
import qualified Data.Vector                    as V
import           Network.Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS


import           Prelude
import qualified SeqMap                         as SM
import           SignalHandlers

cachedir = "cache/"

type Hint = Run (InterpreterT IO)
data Run m = Run { vRequest :: MVar (m ()) }

-- hint gives Either InterpreterError DisplayResult
jsonerror expr err = object ["expr" .= expr, "error" .= err]
jsonresult expr res = object ["expr" .= expr, "result" .= res]

data GHCLive = GHCLive
                { ref       :: MVar [J.Value] -- list of jsonerror or jsonresult Values
                , hint      :: Hint
                , editor    :: Editor
                , getStatic :: Static
                }

data Editor = Editor
              { doc       :: MVar (Document, Map ClientId Client)
              , unique    :: MVar ClientId
              }

data Operation = InsertAfter AtomId AtomId Char
               | Remove AtomId
    deriving (Show, Eq, Ord)

data OpBlock = OpBlock [Operation] deriving (Show, Eq, Ord)

-- inefficiently, [(AtomId,TextAtom)]
type Document = SM.SeqMap AtomId TextAtom

-- Char, and whether this Char has been removed
data TextAtom = TextAtom
    { taCh      :: Char
    , taRemoved :: Maybe Timestamp
    } deriving Show

doc2string seqmap = map taCh $ filter (isNothing . taRemoved)  (map snd (SM.toList seqmap))

emptyDoc = SM.singleton (AtomId (Timestamp 0) (ClientId 0)) (TextAtom ' ' (Just (Timestamp 0)))

newtype Timestamp = Timestamp { unTimestamp :: Integer }
    deriving (Eq, Ord, Show, J.ToJSON, J.FromJSON)

newtype ClientId  = ClientId  { unClientId  :: Integer }
    deriving (Eq, Ord, Show, Enum, J.ToJSON, J.FromJSON)



data Client = Client
              { clientConnected :: Timestamp       -- timestamp when client connected
              , clientId        :: ClientId        -- unique id
              , clientChan      :: Chan J.Value    -- post outgoing messages to this chan
              }


mkTa :: Char -> TextAtom
mkTa ch = TextAtom ch Nothing

-- a Client never makes two IDs with the same timestamp
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

-- staticSiteFiles :: Static
-- staticSiteFiles = $(embed "static")

staticSite :: IO Static
staticSite = staticDevel "static"

-- $(publicFiles "static")
$(staticFiles "static")

mkYesod "GHCLive" [parseRoutes|
/        RootR   GET
/eval    EvalR   GET
/static  StaticR Static getStatic
/loader  LoaderR GET
/edit    EditR   GET
/results ResultsR GET
|]

instance Yesod GHCLive

main :: IO ()
main = do
  -- hint setup
  r  <- newMVar ([] :: [J.Value])
  h  <- newHint
  ss <- staticSite
  -- shared editor setup
  d  <- newMVar (emptyDoc, M.empty)
  u  <- newMVar (ClientId 0)
  let editor = Editor d u
  let master = GHCLive r h editor ss -- staticSiteFiles
      s      = defaultSettings
               { settingsPort = 3000
               , settingsIntercept = WS.intercept (sockets editor)
               }
  runSettings s =<< (toWaiApp master :: IO Yesod.Application)

getLoaderR = do
  y <- getYesod
  cs <- liftIO $ readMVar (doc $ editor y)
  liftIO . putStrLn $ "doc2string result is " ++ doc2string (fst cs )
  t <- liftIO . performHint (hint y) $ moduleHint (doc2string $ fst cs)
  case t of
    Left error -> jsonToRepJson $ cleanShow error
    Right displayres -> jsonToRepJson displayres

getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet|
                            <h1>GHC Live
                            <ul>
                              <li>
                                <a href=@{EditR}>Collaborative Editor
                         |]

getResultsR = do
  y <- getYesod
  h <- liftIO $ readMVar (ref y)
  jsonToRepJson h -- send ALL the state!

getEvalR :: Handler RepJson
getEvalR = do
  y <- getYesod
  expr <- fromMaybe "" <$> lookupGetParam "expr"
  liftIO $ putStr "expression is "
  liftIO $ DTI.putStrLn expr
  -- get Editor with getYesod and then document
  -- get the clients from the editor document (see applyOps for an example)
  (_, clients) <- liftIO $ readMVar (doc $ editor y)
  -- - call pushToClients with the clients and the JSON message you want to send, for example: object [ "refresh" .= True ]
  (t :: Either InterpreterError DisplayResult) <- liftIO . performHint (hint y) $ interpretHint ("display " ++ parens (ST.unpack expr))
  pushToClients clients $ object [ "refreshoutput" .= True ]
  case t of
    Left error -> do
             let jserr = jsonerror expr (cleanShow error)
             liftIO $ modifyMVar_ (ref y) $ \x -> return (x ++ [jserr])
             jsonToRepJson jserr
    Right displayres -> do
             let jsres = jsonresult expr displayres
             liftIO $ modifyMVar_ (ref y) $ \x -> return (x ++ [jsres])
             jsonToRepJson jsres

interpretHint :: (Typeable a, MonadInterpreter m) => String -> m a
interpretHint expr = interpret expr as

moduleHint :: MonadInterpreter m => String -> m [ModuleName]
moduleHint ms = do
  -- save the file
  fname <- liftIO $ cacheFile ms
  let allfiles = ["Helper.hs", fname]
  reset
  loadModules $ map (cachedir ++) allfiles
  ms <- getLoadedModules
  setTopLevelModules ms
  setImports $ ["Prelude", "Network.Web.GHCLive.Display", "Text.Blaze"] ++ ms
  return ms

    -- eval :: String -> Interpret String
    -- eval "something" ~= interpret "(show something) (as :: String)"

{-----------------------------------------------------------------------------
    Interpreter abstraction
------------------------------------------------------------------------------}
newHint :: IO Hint
newHint = newRun $ \a -> void $ runInterpreter (liftIO restoreHandlers >> a)

performHint :: Hint -> InterpreterT IO a -> IO (Either InterpreterError a)
performHint hint act = perform hint $ (Right `liftM` act) `catchError` (return . Left)
{-
loadFile :: Hint -> FilePath -> IO ()
loadFile w filepath = perform w $ do

evaluate :: Hint -> String -> IO String
evaluate w expr = perform w $ do

-- stopInterpreter :: Hint -> IO ()
-}

-- | Thread responsible for "running" a monad that can do IO.
perform :: MonadIO m => Run m -> m a -> IO a
perform run act = do
    ref <- newEmptyMVar
    putMVar (vRequest run) $ do
        a <- act
        liftIO $ putMVar ref a
    takeMVar ref

newRun :: MonadIO m => (m () -> IO ()) -> IO (Run m)
newRun f = do
    vRequest <- newEmptyMVar
    forkIO . f . forever $ do
        act <- liftIO $ takeMVar vRequest
        act
    return Run { vRequest = vRequest }


cleanShow    :: InterpreterError -> String
cleanShow ie = case ie of
                 UnknownError e -> "UnknownError\n" ++ e
                 WontCompile es -> unlines $ map errMsg es
                 NotAllowed e -> "NotAllowed\n" ++ e
                 GhcException e -> "GhcException\n" ++ e

cacheFile f = do
  writeFile (cachedir ++ "Main.hs") f
  return "Main.hs"


{-- shared editor --}
getEditR :: Handler RepHtml
getEditR = defaultLayout $ do
             addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"
             addScript     (StaticR codemirror_lib_codemirror_js)
             addScript     (StaticR codemirror_mode_haskell_haskell_js)
             addScript     (StaticR es6_shim_js)
             addScript     (StaticR document_js)
             addStylesheet (StaticR codemirror_lib_codemirror_css)
             addStylesheet (StaticR foo_css)
             addScript     (StaticR ghclive_js)
             toWidget [lucius|
                         #editor {
                           width: 800px;
                           height: 500px;
                         }
                      |]

             [whamlet|
               <h1>editor
               <form action="#">
                 <textarea #editor>
                 <div #editormessages>
                 <input type=submit value="Load from editor" #load >
               <form action="#">
                 <input #expr >
                 <input type=submit value="Evaluate" #evalit>
                 <br>
                 <input type=submit value="Refresh output" #outputit>
               <div #output >
             |]

insertAtom :: AtomId -> AtomId -> Char -> Document -> Document
insertAtom after aid ch d = go xs
    where
      xs = drop 1 $ SM.toListFrom after d
      go []     = d SM.|> (aid, mkTa ch)
      go ((y,_):ys) | aid < y   = go xs
                    | otherwise = SM.insertBefore y (aid, mkTa ch) d

removeAtom :: AtomId -> Timestamp -> Document -> Document
removeAtom a time = SM.adjust (\(TextAtom ch _) -> TextAtom ch (Just time)) a

-- |'getTimestamp' returns the timestamp in milliseconds
getTimestamp :: MonadIO m => m Timestamp
getTimestamp = liftIO $ liftM (Timestamp . round . (*1000) . utcTimeToPOSIXSeconds) getCurrentTime

{-
collectGarbage :: Integer -> Document -> Document
collectGarbage time d = SM.removeAll ks d
    where
      ks = filter ((< time) . aiTime) (SM.keys d)
-}

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

---


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
