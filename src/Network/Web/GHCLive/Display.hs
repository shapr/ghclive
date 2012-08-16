{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Web.GHCLive.Display where

import           Data.Aeson                    (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson                    as J
import qualified Data.Aeson                    as A
import           Data.Aeson.TH
import qualified Data.Aeson.Types              as J
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

$(deriveJSON id ''DisplayResult)

-- instance ToJSON DisplayResult where
--   toJSON dr = A.object ["result" .= result dr,
--                         "clientType" .= clientType dr,
--                         "resources" .= resources dr
--                        ]



-- instance FromJSON DisplayResult -- requires GHC.Generics
-- instance ToJSON DisplayResult -- requires GHC.Generics
{- since this first hack didn't work, we take another option from
http://hackage.haskell.org/packages/archive/aeson/0.6.0.2/doc/html/Data-Aeson.html#t:FromJSON
instance FromJSON DisplayResult where
    parseJSON (A.Object dr) = DisplayResult {
                                  result = dr .: "result"
                                , clientType = dr .: "clientType"
                                , resources = dr .: "resources"
                                }
    parseJSON _             = mzero
-}
class Display a where
    display :: a -> DisplayResult

displayEmpty = DisplayResult { clientType = "text", resources = [], result = "no result" }

renderMyDiagramToSvg     :: Diagram SVG R2 -> Html
renderMyDiagramToSvg dia = renderDia SVG (SVGOptions "output.file" (Dims 200 200)) (dia :: Diagram Diagrams.Backend.SVG.SVG R2)

instance Display (Diagram SVG R2) where
  display d = DisplayResult { clientType="svg", result=renderHtml $ renderMyDiagramToSvg d, resources = []}

instance Display Text where
  display d = DisplayResult { clientType="text", result= d, resources = []}

instance Display String where
  display d = display $ pack d -- pack to Data.Text

-- overlaps with String instance
-- instance ToMarkup d => Display [d] where
--   display d = display $ mconcat $ Prelude.map toHtml d

instance Display [(Html,Html)] where
  display d = display $ mconcat $ Prelude.map toHtml d

instance Display Markup where
  display d = DisplayResult{clientType="svg", result= renderHtml $ p d, resources =[] }

instance (ToMarkup a, ToMarkup b) => Display (a,b) where
  display (a,b) = display $ mconcat [toMarkup a,toMarkup b]
-- Other useful instances?

instance (ToMarkup a, ToMarkup b) => ToMarkup (a,b) where
  toMarkup (a,b) = mconcat [toMarkup a,toMarkup b]
