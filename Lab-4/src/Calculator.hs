-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import Data.Maybe
import Data.IORef
import System.IO.Unsafe



canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

-- Creating a "global" variable
-- which we can change in the zoom functions
-- to be able to rescale the window
canScale :: IORef Double
canScale = unsafePerformIO $ newIORef 0.04


main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas     <- mkCanvas canWidth canHeight   -- The drawing area
     fx         <- mkHTML "<i> f </i>(<i>x</i>) = "  -- The text "f(x)="
     nline      <- mkHTML "<br>"  
     input      <- mkInput 20 " input formula"    -- The formula input
     draw       <- mkButton "Draw graph"         -- The draw button
     inZoom     <- mkButton "Zoom in"            -- The zoom buttons (added these last 2)
     outZoom    <- mkButton "Zoom out"
     diff       <- mkButton "Differentiate"      -- The diff button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input, pure draw, 
      pure diff]
     zoom <- row [pure inZoom, pure outZoom]
     getBody window #+ [column [pure canvas, pure zoom, pure nline, pure formula]] 

     -- Styling
     getBody window # set style [("backgroundColor","#2b3d51"),
                                 ("textAlign","center"), 
                                 ("color","#FFF")]
     pure zoom  # set style [("paddingLeft", "35%")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $    \ _ -> readAndDraw input canvas
     on valueChange' input $    \ _ -> readAndDraw input canvas
     on UI.click     diff  $    \ _ -> diffAndDraw input canvas
     on UI.click     inZoom $   \ _ -> zoomAndDraw input canvas False -- False if in zoom
     on UI.click     outZoom $  \ _ -> zoomAndDraw input canvas True  -- True if out zoom 

  
readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.

     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText (show $ simplify $ fromJust $ readExpr formula) (10,290) canvas
     scaleValue <- liftIO $ readIORef canScale
     
     case (readExpr formula) of
      Just e -> path "#334960" (points e scaleValue (canHeight, canWidth)) canvas
      otherwise -> UI.fillText "Error" (canWidth/2,canHeight/2) canvas
  
diffAndDraw :: Element -> Canvas -> UI ()
diffAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
      formula <- get value input
      -- Clear the canvas
      clearCanvas canvas
      -- The following code draws the formula text in the canvas and a blue line.
      -- It should be replaced with code that draws the graph of the function.

      scaleValue <- liftIO $ readIORef canScale
      set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
      UI.fillText (diffExp formula) (10,290) canvas
      element input # set UI.value (diffExp formula)

      case (readExpr formula) of
        Just e -> path "#334960" (points (differentiate e) scaleValue (canHeight, canWidth)) canvas
        otherwise -> UI.fillText "Error" (canWidth/2,canHeight/2) canvas 
      
      where
        diffExp f = showExpr (differentiate (fromJust (readExpr f)))

zoomAndDraw :: Element -> Canvas -> Bool -> UI ()
zoomAndDraw input canvas zoomInOrOut =
  do -- Get the current formula (a String) from the input element
      formula <- get value input
      -- Clear the canvas
      clearCanvas canvas
      -- The following code draws the formula text in the canvas and a blue line.
      -- It should be replaced with code that draws the graph of the function.
      oldScale <- liftIO $ readIORef canScale

      if zoomInOrOut == True 
        then do liftIO $ writeIORef canScale (oldScale*1.5)
        else do liftIO $ writeIORef canScale (oldScale/1.5)
      
      set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
      UI.fillText (show $ simplify $ fromJust $ readExpr formula) (10,290) canvas
      newScale <- liftIO $ readIORef canScale
      case (readExpr formula) of
        Just e -> path "#334960" (points e newScale (canHeight, canWidth)) canvas
        otherwise -> UI.fillText "Error" (canWidth/2,canHeight/2) canvas


-------------------------------------------------------------
-- *H
-------------------------------------------------------------

-- Calculates all the points as well as rounding them before converting them
-- back to double for the graph reader to use 
points :: Expr -> Double -> (Int , Int) -> [Point]
points e scale (width, height) = 
  [(z, fromIntegral (round (realToPix (eval e (pixToReal z))))) | z <- [0..dw]]  where

  dw = fromIntegral width
  dh = fromIntegral height
  -- converts a pixel x-coordinate to a real x-coordinate 
  pixToReal :: Double -> Double 
  pixToReal x = scale*(x - dw / 2)
  -- Has to be scaled with scale

  -- converts a real y-coordinate to a pixel y-coordinate 
  realToPix :: Double -> Double 
  realToPix y = (dh / 2) - (y / scale)
  -- Has to be scaled by y/scale (inverse)