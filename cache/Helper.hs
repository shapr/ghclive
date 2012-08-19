module Helper where
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Network.Web.GHCLive.Display

dia :: Diagram Diagrams.Backend.SVG.SVG R2 -> Diagram Diagrams.Backend.SVG.SVG R2
dia = id
