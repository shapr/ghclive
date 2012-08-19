{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
-- {-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Web.GHCLive.Display where

import           Data.Aeson                    (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson                    as J
import qualified Data.Aeson                    as A
import           Data.Aeson.TH
import qualified Data.Aeson.Types              as J
import           Data.Char
import           Data.Word
import           Data.Int
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Typeable
import qualified Diagrams.Backend.SVG          as D
import qualified Diagrams.Prelude              as D
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5              as B
import           Prelude hiding (span)

data ClientType = Html | Svg | Text deriving (Eq, Show, Enum)

instance J.ToJSON ClientType where toJSON = J.toJSON . map toLower . show

newtype DisplayResult = DisplayResult [DR] deriving (Eq, Monoid, Typeable, ToJSON)

-- instance ToJSON DisplayResult where toJSON (DisplayResult rs) = J.object [ T.pack "results" .= rs ]

data DR = DR {
      clientType :: ClientType, -- "SVG" "IMG" etc, changes how the browser-side javascript handles this result.
      result :: TL.Text            -- actual result data
      } deriving (Eq, Show, Typeable)

instance ToJSON DR where toJSON (DR c r) = J.object [ T.pack "t" .= c, T.pack "r" .= r ]

text :: TL.Text -> DisplayResult
text x = DisplayResult [ DR Text x ]

html :: B.Markup -> DisplayResult
html x = DisplayResult [ DR Html (renderHtml x) ]

svg :: B.Markup -> DisplayResult
svg x = DisplayResult [ DR Svg (renderHtml x) ]

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
displayEmpty = DisplayResult []

renderMyDiagramToSvg :: Double -> D.Diagram D.SVG D.R2 -> B.Html
renderMyDiagramToSvg size dia = 
   D.renderDia D.SVG (D.SVGOptions "output.file" (D.Dims size size)) dia

instance Display DisplayResult where
  display d = d

instance (a ~ D.SVG, b ~ D.R2) => Display (D.Diagram a b) where
  display d      = svg (renderMyDiagramToSvg 150 d)
  displayList ds = displayList $ map (svg . renderMyDiagramToSvg 75) ds

instance Display TL.Text where
  display d = text d

instance Display T.Text where
  display d = text (TL.fromStrict d)

instance Display a => Display [a] where
  display = displayList

instance Display B.Markup where
  display d = html d

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
  displayList = display . TL.pack
instance Display ()
instance Show a => Display (Maybe a)
instance Show a => Display (Sum a)
instance Show a => Display (Product a)
instance Show a => Display (First a)
instance Show a => Display (Last a)

