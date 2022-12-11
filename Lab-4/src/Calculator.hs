-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300


main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas     <- mkCanvas canWidth canHeight   -- The drawing area
     fx         <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input      <- mkInput 20 "x"                -- The formula input
     draw       <- mkButton "Draw graph"         -- The draw button
     inZoom     <- mkButton "Zoom in"            -- The zoom buttons (added these last 2)
     outZoom    <- mkButton "Zoom out"
     diff       <- mkButton "Differentiate"      -- The diff button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw, 
      pure inZoom, pure diff]] -- <-- I added these

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas
     on UI.click     diff  $ \ _ -> readAndDraw input canvas -- <- need to
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
     if readExpr(formula) == Nothing then UI.fillText "Wrong formula" (10,canHeight/2) canvas else path "blue" (points (readExpr(formula))) canvas
  


-------------------------------------------------------------
-- *H
-------------------------------------------------------------
width = 300
height = 300
scale = 0.04

points :: Expr -> Double -> (Int , Int) -> [Point]
points e scale (width, height) = 
  [(z, realToPix (eval e (pixToReal z))) | z <- [0..dh]]  where
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
  -- The scale used is 0.04 so keep that in mind
  -- Will probably become useful when 
  -- Real values ranges from -6 -> 6 for both x and y
  -- Canvas values goes from 0 -> 300 for both x and y
  -- These are in the example but it's suppose to be general
  -- Otherwise the zoom function won't work

-------------------------------------------------------------
-- *J
-------------------------------------------------------------
-- Also implement zooming

-------------------------------------------------------------
-- *K
-------------------------------------------------------------
{-The recommended solution is to let the differentiated expression 
replace the expression in the text entry field. 
This allows you to differentiate many times, 
and provides a nice way to test that showExpr and simplify 
work as expected.)-}

-- that is it!
