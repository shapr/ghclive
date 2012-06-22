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
import qualified Data.Text as ST
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Text.Blaze.Renderer.Text (renderMarkup)

main :: IO()
main = scotty 3000 $ do
     hint <- liftIO $ newHint

     middleware logStdoutDev
     -- middleware $ staticPolicy (noDots >-> addBase "static") -- serves jquery.js and clock.js from static/
     middleware $ staticRoot "static"

     get "/" $ file "hint.html"

     get "/hint" $ do
         u <- param "fileurl"
         e <- param "expr"
         t <- liftIO . performHint hint $ runHint e u
         case t of
             Left error -> json $ A.object ["result" .= cleanShow error, "expr" .= e]
             Right string -> json $ A.object ["result" .= renderMarkup string, "expr" .= e]
         -- json t

-- runHint :: String -> String -> m String inferred
-- runHint :: String -> String -> InterpreterT IO String
runHint expr fileurl = do
    doc <- liftIO $ do
        Right doc <- openURI fileurl
        BS.writeFile "Main.hs" doc
    loadModules ["Main.hs","Helper.hs"]
    setTopLevelModules ["Main","Helper"]
    setImportsQ [("Prelude",Nothing)]
    -- result <- eval expr
    result <- interpret expr (as :: H.Markup)
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


{-- explicit SVG value to render, for jquery-console testing
-- can load and eval the SVG in the next prototype -}

spike :: Trail R2
spike = fromOffsets . map r2 $ [(1,3), (1,-3)]

burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike

colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]

example :: Diagram Diagrams.Backend.SVG.SVG R2
example = lw 1.0 . mconcat  . zipWith lc colors . map stroke . explodeTrail origin $ burst

bar = renderDia SVG (SVGOptions "output.file" (Dims 200 200)) example
