{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
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
import           GHC.Generics
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
displayString = text . TL.pack

displayChar :: Char -> DisplayResult
displayChar = displayString . return

displayListOf :: (a -> DisplayResult) ->  [a] -> DisplayResult
displayListOf _     []     = display "[]"
displayListOf showx (x:xs) = display "[" <> showx x <> showl xs
  where
    showl []     = display "]"
    showl (y:ys) = display "," <> showx y <> showl ys

class GDisplay f where
  gdisplay :: f a -> DisplayResult

instance GDisplay U1 where
  gdisplay U1 = mempty

instance Display a => GDisplay (K1 i a) where
  gdisplay (K1 a) = display a

instance (GDisplay f, GDisplay g) => GDisplay (f :+: g) where
  gdisplay (L1 f) = gdisplay f
  gdisplay (R1 g) = gdisplay g

instance (GDisplay f, GDisplay g) => GDisplay (f :*: g) where
  gdisplay (f :*: g) = gdisplay f <> displayChar ' ' <> gdisplay g

instance (Constructor c, GDisplay f) => GDisplay (M1 C c f) where
  gdisplay m@(M1 x) = displayString (conName m) <> displayChar ' ' <> gdisplay x

instance GDisplay f => GDisplay (M1 S c f) where
  gdisplay (M1 x) = gdisplay x

instance GDisplay f => GDisplay (M1 D c f) where
  gdisplay (M1 x) = gdisplay x

class Display a where
    displayList :: [a] -> DisplayResult
    displayList = displayListOf display

    display :: a -> DisplayResult
    default display :: (Generic a, GDisplay (Rep a)) => a -> DisplayResult
    display = gdisplay . from

{-
    default display :: Show a => a -> DisplayResult
    display = display . show
-}

displayEmpty :: DisplayResult
displayEmpty = DisplayResult []

renderMyDiagramToSvg :: Double -> D.Diagram D.SVG D.R2 -> B.Html
renderMyDiagramToSvg size dia =
   D.renderDia D.SVG (D.SVGOptions "output.file" (D.Dims size size)) dia

instance Display DisplayResult where
  display d = d

instance (a ~ D.SVG, b ~ D.R2) => Display (D.Diagram a b) where
  display     = svg . renderMyDiagramToSvg 150
  displayList = displayListOf (svg . renderMyDiagramToSvg 75)

instance Display TL.Text where
  display d = displayChar '"' <> text d <> displayChar '"'

instance Display T.Text where
  display d = displayChar '"' <> text (TL.fromStrict d) <> displayChar '"'

instance Display a => Display [a] where
  display = displayList

instance Display B.Markup where
  display d = html d

instance (Display a, Display b) => Display (a,b) where
  display (a, b) = displayChar '(' <> display a <> displayChar ',' <> display b <> displayChar ')'

instance (Display a, Display b, Display c) => Display (a,b,c) where
  display (a, b, c) = displayChar '(' <> display a <> displayChar ',' <> display b <> displayChar ',' <> display c <> displayChar ')'

instance Display Int where display = displayString . show
instance Display Int8 where display = displayString . show
instance Display Int16 where display = displayString . show
instance Display Int32 where display = displayString . show
instance Display Int64 where display = displayString . show
instance Display Word where display = displayString . show
instance Display Word8 where display = displayString . show
instance Display Word16 where display = displayString . show
instance Display Word32 where display = displayString . show
instance Display Word64 where display = displayString . show
instance Display Integer where display = displayString . show
instance Display Float where display = displayString . show
instance Display Double where display = displayString . show
instance Display Char where
  display = displayString . show
  displayList = display . TL.pack
instance Display () where display () = displayString "()"

-- generic instances
instance Display a => Display (Maybe a)
instance (Display a, Display b) => Display (Either a b)
