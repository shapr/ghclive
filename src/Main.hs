{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, TypeFamilies #-}
module Main where
import           Control.Monad.Trans
import qualified Data.ByteString                      as BS
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid
import qualified Data.Text.Lazy                       as T
import           Language.Haskell.Interpreter         hiding (get)
import           Network.Curl.Download
import qualified Text.Blaze.Html5                     as H
import           Text.Blaze.Html5.Attributes          (class_, href, rel, src, type_)
import           Yesod
import           Yesod.Static
import           Network.Wai.Handler.Warp (runSettings, Settings(..), defaultSettings)

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                        (forever, liftM, void)
import           Control.Monad.Error.Class
import           Data.Aeson                           ((.=))
import qualified Data.Aeson                           as A
import           Data.Char                            (isUpper)
import qualified Data.Text                            as ST
import           Data.Typeable
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Network.Web.GHCLive.Display
import           Text.Blaze
import           Text.Blaze.Renderer.Text             (renderMarkup)



cachedir = "cache/"

type Hint = Run (InterpreterT IO)
data Run m = Run { vRequest :: MVar (m ()) }

data GHCLive = GHCLive
                { ref       :: MVar H.Html
                , hint      :: Hint
                , getStatic :: Static
                }

staticSite = static "static"
$(staticFiles "static")

mkYesod "GHCLive" [parseRoutes|
/       RootR   GET
/output OutputR GET
/eval   EvalR   GET
/load   LoadR   POST
/static StaticR Static getStatic
|]

instance Yesod GHCLive

-- (modifyMVar_) :: MVar a -> (a -> IO a) -> IO ()
happend :: String -> H.Html -> (H.Html -> IO H.Html)
happend expr adds content = return $ mconcat [content,
                                              H.div "hint> " ! class_ "hint-prompt",
                                              H.div (H.toMarkup expr) ! class_ "hint-expr",
                                              H.p $ H.div adds ! class_ "hint-res"
                                              ]

main :: IO ()
main = do
  r  <- newMVar defaultOutput
  h  <- newHint
  st <- staticSite
  runSettings defaultSettings =<< toWaiApp (GHCLive r h st)

getRootR :: Handler RepHtml
getRootR = sendFile typeHtml "static/hint.html"

getOutputR :: Handler RepHtml
getOutputR = do
  y <- getYesod
  h <- liftIO $ readMVar (ref y)
  defaultLayout $ do
    addScript (StaticR jquery_js)
    addScriptRemote "http://localhost:9090/bdo"
    setTitle "ghclive output"
    [whamlet|
      <p>ghclive output
      #{h}
    |]

getEvalR :: Handler RepJson
getEvalR = do
  y <- getYesod
  e <- ST.unpack . fromMaybe "" <$> lookupGetParam "expr"
  t <- liftIO . performHint (hint y) $ interpretHint e
  case t of
    Left error -> do
             liftIO $ modifyMVar_ (ref y) (happend e (H.toMarkup $ cleanShow error))
             jsonToRepJson . display $ cleanShow error
    Right displayres -> do
             liftIO $ modifyMVar_ (ref y) (happend e displayres)
             jsonToRepJson $ display displayres

postLoadR :: Handler RepJson
postLoadR = do
  y <- getYesod
  f <- ST.unpack . fromMaybe "" <$> lookupPostParam "editor"
  t <- liftIO . performHint (hint y) $ moduleHint f
  case t of
    Left error -> do
             jsonToRepJson $ cleanShow error
    Right displayres -> do
             jsonToRepJson displayres

interpretHint :: (Typeable a, MonadInterpreter m) => String -> m a
interpretHint expr = do
  interpret expr as

moduleHint :: MonadInterpreter m => String -> m [ModuleName]
moduleHint ms = do
  -- save the file
  fname <- liftIO $ cacheFile ms
  let allfiles = "Helper.hs" : [fname]
  loadModules $ map (cachedir ++) allfiles
  setTopLevelModules $ map (takeWhile (/= '.')) allfiles
  setImportsQ $ [("Prelude",Nothing),("Network.Web.GHCLive.Display",Nothing),("Text.Blaze",Nothing)] ++ [(fname,Nothing)]
  getLoadedModules
  -- json $ display $ "Loaded module " ++ fname

runHint :: MonadInterpreter m => String -> String -> m H.Html
runHint expr fileurl = do
  files <- liftIO $ do
    putStrLn $ "fileurl is " ++ show (lines fileurl)
    mapM cacheUrl (urls fileurl)
  let allfiles = "Helper.hs" : files
  loadModules $ map (cachedir ++) allfiles
  setTopLevelModules $ map (takeWhile (/= '.')) allfiles
  let imports = map (flip (,) Nothing) $ mods fileurl
  setImportsQ $ [("Prelude",Nothing),("Network.Web.GHCLive.Display",Nothing),("Text.Blaze",Nothing)] ++ imports
  interpret expr as

    -- eval :: String -> Interpret String
    -- eval "something" ~= interpret "(show something) (as :: String)"

{-----------------------------------------------------------------------------
    Interpreter abstraction
------------------------------------------------------------------------------}
newHint :: IO Hint
newHint = newRun $ void . runInterpreter

performHint :: Hint -> InterpreterT IO a -> IO (Either InterpreterError a)
performHint hint act = perform hint $ do
                         (Right `liftM` act) `catchError` (return . Left) -- can no longer do C-c !
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
    return $ Run { vRequest = vRequest }


cleanShow    :: InterpreterError -> String
cleanShow ie = case ie of
                 UnknownError e -> "UnknownError\n" ++ e
                 WontCompile es -> unlines $ map errMsg es
                 NotAllowed e -> "NotAllowed\n" ++ e
                 GhcException e -> "GhcException\n" ++ e


{--- output page goodies ---}
defaultOutput :: H.Html
defaultOutput = mempty

filenameFromUrl = reverse . takeWhile (/= '/') . reverse

cacheUrl fileurl = do
  putStrLn ("cacheFile got " ++ fileurl)
  Right doc <- openURI fileurl
  BS.writeFile (cachedir ++ fname) doc -- is wrong if filename /= module name!
  return fname
      where fname = filenameFromUrl fileurl

cacheFile f = do
  writeFile (cachedir ++ fname ++ ".hs") f
  return fname
    where fname = filenameFromText f

-- this ugly hack WILL fail with PRAGMAs at the top of the file! fix it later!
filenameFromText f = head . take 1 . drop 1 . words . head . take 1 $ lines f

listmatch a b = and $ zipWith (==) a b

urls fbox = filter (listmatch "http://") $ lines fbox

mods fbox = filter (isUpper . head) $ lines fbox -- blows up with empty lines?
