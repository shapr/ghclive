{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Web.GHCLive.Display where

import           Data.Aeson ((.=), ToJSON)
import qualified Data.Aeson as A
import           Data.Text.Lazy
import           Data.Typeable
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5

data DisplayResult = DisplayResult {
      clientType :: String, -- "SVG" "IMG" etc, changes how the browser-side javascript handles this result.
      resources :: [String], -- images get loaded from the server
      result :: Text -- actual result data
      -- Luite has type for Haskell type?
      } deriving (Eq, Show, Typeable)

instance ToJSON DisplayResult where
  toJSON dr = A.object ["result" .= result dr,
                        "clientType" .= clientType dr,
                        "resources" .= resources dr
                       ]

class Display a where
    display :: (Display a) => a -> DisplayResult

displayEmpty = DisplayResult { clientType = "text", resources = [], result = "no result" }

renderMyDiagramToSvg     :: Diagram SVG R2 -> Text
renderMyDiagramToSvg dia = renderHtml $ renderDia SVG (SVGOptions "output.file" (Dims 200 200)) (dia :: Diagram Diagrams.Backend.SVG.SVG R2)

instance Display (Diagram SVG R2) where
    display d = DisplayResult { clientType="svg", result=renderMyDiagramToSvg d, resources = []}

instance Display Text where
    display d = DisplayResult { clientType="text", result= d, resources = []}

instance Display String where
    display d = display $ pack d -- pack to Data.Text

instance Display Markup where
    display d = DisplayResult{clientType="svg", result= renderHtml d, resources =[] }

-- Other useful instances?