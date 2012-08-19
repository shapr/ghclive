{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
-- {-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DefaultSignatures    #-}

module Network.Web.GHCLive.Display where

import           Data.Aeson                    (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson                    as J
import qualified Data.Aeson                    as A
import           Data.Aeson.TH
import qualified Data.Aeson.Types              as J
import           Data.Word
import           Data.Int
import           Data.Monoid
import           Data.Text.Lazy hiding (span)
import           Data.Typeable
import           Diagrams.Backend.SVG
import           Diagrams.Prelude hiding ((<>), First, Last)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5
import           Prelude hiding (span)

data DisplayResult = DisplayResult {
      clientType :: String, -- "SVG" "IMG" etc, changes how the browser-side javascript handles this result.
      resources :: [String], -- images get loaded from the server
      result :: Text -- actual result data
      -- Luite has type for Haskell type?
      } deriving (Eq, Show, Typeable)

instance Monoid DisplayResult where
  mempty = DisplayResult "text" [] mempty
  mappend (DisplayResult "svg" rs t) (DisplayResult "svg" rs' t') = DisplayResult "svg" (rs ++ rs') (t <> t')
  mappend (DisplayResult "text" rs t) (DisplayResult "text" rs' t') = DisplayResult "text" (rs ++ rs') (t <> t')
  mappend (DisplayResult "text" rs t) (DisplayResult "svg" rs' t') = DisplayResult "svg" (rs ++ rs') (renderHtml (span (toMarkup t)) <> t')
  mappend (DisplayResult "svg" rs t) (DisplayResult "text" rs' t') = DisplayResult "text" (rs ++ rs') (t <> renderHtml (span (toMarkup t)))

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

displayString :: String -> DisplayResult
displayString = display

displayListOf :: (a -> DisplayResult) ->  [a] -> DisplayResult
displayListOf _     []     = display "[]"
displayListOf showx (x:xs) = display "[" <> showx x <> showl xs
  where
    showl []     = display "]"
    showl (y:ys) = display "," <> showx y <> showl ys

class Display a where
    display :: a -> DisplayResult
    default display :: Show a => a -> DisplayResult
    display = display . show
    displayList :: [a] -> DisplayResult
    displayList = displayListOf display

displayEmpty :: DisplayResult
displayEmpty = DisplayResult { clientType = "text", resources = [], result = pack "no result" }

renderMyDiagramToSvg     :: Diagram SVG R2 -> Html
renderMyDiagramToSvg dia = renderDia SVG (SVGOptions "output.file" (Dims 200 200)) (dia :: Diagram Diagrams.Backend.SVG.SVG R2)

instance (a ~ SVG, b ~ R2) => Display (Diagram a b) where
  display d = DisplayResult { clientType="svg", result=renderHtml $ renderMyDiagramToSvg d, resources = []}

instance Display Text where
  display d = DisplayResult { clientType="text", result= d, resources = []}

instance Display a => Display [a] where
  display = displayList

instance Display Markup where
  display d = DisplayResult{clientType="svg", result= renderHtml $ p d, resources =[] }

instance (Display a, Display b) => Display (a,b) where
  display (a, b) = display "(" <> display a <> display "," <> display b <> display ")"

instance (Display a, Display b, Display c) => Display (a,b,c) where
  display (a, b, c) = display "(" <> display a <> display "," <> display b <> display "," <> display c <> display ")"

instance Display Int
instance Display Int8
instance Display Int16
instance Display Int32
instance Display Int64
instance Display Word
instance Display Word8
instance Display Word16
instance Display Word32
instance Display Word64
instance Display Integer
instance Display Float
instance Display Double
instance Display Char where
  displayList = display . pack
instance Display ()
instance Show a => Display (Maybe a)
instance Show a => Display (Sum a)
instance Show a => Display (Product a)
instance Show a => Display (First a)
instance Show a => Display (Last a)

-- overlaps with String instance
-- instance ToMarkup d => Display [d] where
--   display d = display $ mconcat $ Prelude.map toHtml d

--instance Display [(Html,Html)] where
--  display d = display $ mconcat $ Prelude.map toHtml d

--instance (ToMarkup a, ToMarkup b) => Display (a,b) where
--  display (a,b) = display $ mconcat [toMarkup a,toMarkup b]
-- Other useful instances?

-- instance (ToMarkup a, ToMarkup b) => ToMarkup (a,b) where
--  toMarkup (a,b) = mconcat [toMarkup a,toMarkup b]

