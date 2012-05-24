{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.Trans
import           Data.Monoid
import qualified Data.Text.Lazy as T
import           Data.Time.Clock
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import qualified Text.Blaze.Html5 as H
import           Web.Scotty

main :: IO()
main = scotty 3000 $ do
     middleware logStdoutDev
     middleware $ staticRoot "static" -- serves jquery.js and clock.js from static/

     get "/" $ file "clock.html"

     get "/clock" $ do
         t <- liftIO getCurrentTime 
         json t