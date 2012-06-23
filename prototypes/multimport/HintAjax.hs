{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.Trans
import           Data.Monoid
import qualified Data.Text.Lazy as T
-- import           Data.Time.Clock
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import qualified Text.Blaze.Html5 as H
import           Web.Scotty
import           Language.Haskell.Interpreter hiding (get)
import           Network.Curl.Download
import qualified Data.ByteString as BS

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad (void, forever, liftM)
import           Control.Monad.Error.Class
import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import           Data.Char (isUpper)
import qualified Data.Text as ST
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Text.Blaze.Renderer.Text (renderMarkup)

cachedir = "cache/"

main :: IO()
main = scotty 3000 $ do
     hint <- liftIO $ newHint

     middleware logStdoutDev -- serves jquery.js and clock.js from static/
     -- middleware $ staticPolicy (noDots >-> addBase "static") -- serves jquery.js and clock.js from static/ with scotty > 3
     middleware $ staticRoot "static"

     get "/" $ file "hint.html"

     get "/hint" $ do
         u <- param "fileurl"
         e <- param "expr"
         t <- liftIO . performHint hint $ runHint e u
         case t of
             Left error -> json $ A.object ["result" .= cleanShow error, "expr" .= e]
             -- Right string -> json $ A.object ["result" .= renderMarkup string, "expr" .= e]
             Right string -> json $ A.object ["result" .= string, "expr" .= e]
         -- json t

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

-- runHint :: String -> String -> m String inferred
-- runHint :: String -> String -> InterpreterT IO String
runHint expr fileurl = do
  -- (webs,mods) =
  files <- liftIO $ do
             putStrLn ("fileurl is " ++ (show $ lines fileurl))
             mapM cacheFile (urls fileurl)
  let allfiles = ["Helper.hs"] ++ files
  loadModules $ map (cachedir ++) allfiles
  setTopLevelModules $ map (takeWhile (/= '.')) allfiles
  let imports = map (flip (,) Nothing) $ mods fileurl
  setImportsQ $ [("Prelude",Nothing)] ++ imports
  result <- eval expr
  -- result <- interpret expr (as :: H.Markup)
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
