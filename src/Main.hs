{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.Trans
import qualified Data.ByteString                      as BS
import           Data.Monoid
import qualified Data.Text.Lazy                       as T
import           Language.Haskell.Interpreter         hiding (get)
import           Network.Curl.Download
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import qualified Text.Blaze.Html5                     as H
import           Web.Scotty

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

main :: IO()
main = do
  ref <- newMVar defaultOutput
  scotty 3000 $ do
     hint <- liftIO $ newHint
     middleware logStdoutDev -- serves jquery.js and clock.js from static/
     middleware $ staticRoot "static"

     get "/" $ file "static/hint.html"

     get "/output" $ do
                  h <- liftIO $ readMVar ref
                  html . renderMarkup $ wrap h

     get "/hint" $ do
         u <- param "fileurl"
         e <- param "expr"
         t <- liftIO . performHint hint $ runHint e u
         case t of
           Left error -> do
                     liftIO $ modifyMVar_ ref (happend e (H.toMarkup $ cleanShow error))
                     json . display $ cleanShow error
           Right displayres -> do
                     liftIO $ modifyMVar_ ref (happend e displayres)
                     json $ display displayres

-- (modifyMVar_) :: MVar a -> (a -> IO a) -> IO ()
happend :: String -> H.Html -> (H.Html -> IO H.Html)
happend expr adds content = return $ content `mappend` (H.p $ H.toMarkup ("hint> " :: String) `mappend` H.toMarkup expr `mappend` H.p adds)

{---
output page goodies
---}
defaultOutput :: H.Html
defaultOutput = mempty

wrap c = H.docTypeHtml $ do
           H.head $ do
             H.title $ H.toMarkup ("ghclive output" :: String)
           H.body $ do
             H.p $ H.toMarkup ("ghclive output" :: String)
             c

filenameFromUrl = reverse . takeWhile (/= '/') . reverse

cacheFile fileurl = do
  putStrLn ("cacheFile got " ++ fileurl)
  Right doc <- openURI fileurl
  BS.writeFile (cachedir ++ fname) doc -- is wrong if filename /= module name!
  return fname
      where fname = filenameFromUrl fileurl

listmatch a b = and $ zipWith (==) a b

urls fbox = filter (listmatch "http://") $ lines fbox

mods fbox = filter (isUpper . head) $ lines fbox -- blows up with empty lines?

runHint :: MonadInterpreter m => String -> String -> m H.Html
runHint expr fileurl = do
  files <- liftIO $ do
             putStrLn ("fileurl is " ++ (show $ lines fileurl))
             mapM cacheFile (urls fileurl)
  let allfiles = ["Helper.hs"] ++ files
  loadModules $ map (cachedir ++) allfiles
  setTopLevelModules $ map (takeWhile (/= '.')) allfiles
  let imports = map (flip (,) Nothing) $ mods fileurl
  setImportsQ $ [("Prelude",Nothing),("Network.Web.GHCLive.Display",Nothing),("Text.Blaze",Nothing)] ++ imports
  --result <- interpret ("display " ++ parens expr) as -- (as :: DisplayResult)
  result <- interpret expr as -- (as :: DisplayResult)
  return result

    -- eval :: String -> Interpret String
    -- eval "something" ~= interpret "(show something) (as :: String)"

{-----------------------------------------------------------------------------
    Interpreter abstraction
------------------------------------------------------------------------------}
type Hint = Run (InterpreterT IO)

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
data Run m = Run { vRequest :: MVar (m ()) }

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
                 UnknownError e -> ("UnknownError\n" ++ e)
                 WontCompile es -> unlines $ (map errMsg es)
                 NotAllowed e -> ("NotAllowed\n" ++ e)
                 GhcException e -> ("GhcException\n" ++ e)
