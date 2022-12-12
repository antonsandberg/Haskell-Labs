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

canScale :: IORef Double
canScale = unsafePerformIO $ newIORef 0.04


main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas     <- mkCanvas canWidth canHeight   -- The drawing area
     fx         <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input      <- mkInput 20 "input formula"    -- The formula input
     draw       <- mkButton "Draw graph"         -- The draw button
     inZoom     <- mkButton "Zoom in"            -- The zoom buttons (added these last 2)
     outZoom    <- mkButton "Zoom out"
     diff       <- mkButton "Differentiate"      -- The diff button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw, 
      pure inZoom, pure outZoom, pure diff]] -- <-- I added these

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $    \ _ -> readAndDraw input canvas
     on valueChange' input $    \ _ -> readAndDraw input canvas
     on UI.click     diff  $    \ _ -> diffAndDraw input canvas
     on UI.click     inZoom $   \ _ -> zoomAndDraw input canvas False -- False if in zoom
     on UI.click     outZoom $  \ _ -> zoomAndDraw input canvas True  -- True if out zoom True False-- <- need to
                                                                -- CHANGE THIS


readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.


     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     -- path "blue" [(10,10),(canWidth-10,canHeight/2)] canvas
     scaleValue <- liftIO $ readIORef canScale
     
     -- let readE = fromMaybe $ readExpr formula
     case (readExpr formula) of
      Just e -> path "blue" (points e scaleValue (canHeight, canWidth)) canvas
      otherwise -> UI.fillText "Error" (10,canHeight/2) canvas

      
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
      UI.fillText (diffExp formula) (10,canHeight/2) canvas
      element input # set UI.value (diffExp formula)
      --set UI.input # set UI.type_ showExpr (differentiate (fromJust (readExpr f)))

      case (readExpr formula) of
        Just e -> path "red" (points (differentiate e) scaleValue (canHeight, canWidth)) canvas
        otherwise -> UI.fillText "Error" (10,canHeight/2) canvas 
      
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
      UI.fillText formula (10,canHeight/2) canvas
      newScale <- liftIO $ readIORef canScale
      case (readExpr formula) of
        Just e -> path "green" (points e newScale (canHeight, canWidth)) canvas
        otherwise -> UI.fillText "Error" (10,canHeight/2) canvas


-------------------------------------------------------------
-- *H
-------------------------------------------------------------

-- Calculates all the points as well as rounding them before converting them
-- back to double
points :: Expr -> Double -> (Int , Int) -> [Point]
points e scale (width, height) = 
  [(z, fromIntegral (round (realToPix (eval e (pixToReal z))))) | z <- [0..dh]]  where
  dh = fromIntegral width
  
  -- converts a pixel x-coordinate to a real x-coordinate 
  pixToReal :: Double -> Double 
  pixToReal x = scale*(x - dh / 2)
  -- Has to be scaled with scape

  -- converts a real y-coordinate to a pixel y-coordinate 
  realToPix :: Double -> Double 
  realToPix y = (fromIntegral height / 2) - (y / scale)
  -- Has to be scaled by y/scale (inverse)

  -- (-6, 6) -> (0,0)
  -- (6, 6)  -> (300, 0)
  -- (-6, 6) -> (0, 300)
  -- (6, -6) -> (300, 300)
  -- (0, 0)  -> (150, 150)

