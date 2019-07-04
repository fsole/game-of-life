import Graphics.UI.GLUT
import System.Random
import Data.IORef
import Control.Concurrent

data Grid = Grid Int Int [Int]

gridWidth :: Grid -> Int
gridWidth (Grid width _ _ ) = width

gridHeight :: Grid -> Int
gridHeight (Grid _ height _ ) = height

gridData :: Grid -> [Int]
gridData (Grid _ _ list ) = list

gridAt :: Grid -> Int -> Int -> Int
gridAt grid x y
    | indexIsValid = (gridData grid) !! index 
    | otherwise    = 0
    where index = (x + y * gridWidth grid )
          indexIsValid = x >= 0 && x < gridWidth grid && y >= 0 && y < gridHeight grid

cellNeighbors :: Grid -> Int -> Int -> Int
cellNeighbors grid x y = (gridAt grid (x-1) (y+1)) + (gridAt grid x (y+1)) + (gridAt grid (x+1) (y+1)) +
                         (gridAt grid (x-1) y)     +                         (gridAt grid (x+1) y)   +
                         (gridAt grid (x-1) (y-1)) + (gridAt grid x (y-1)) + (gridAt grid (x+1) (y-1))

gridPoints :: Grid -> [(GLfloat,GLfloat,GLfloat)]
gridPoints grid = (map fst ( filter (\x -> snd x > 0) (zip (generatePoints (gridWidth grid - 1) (gridHeight grid - 1) ) (gridData grid) ) ))
                   where generatePoints width height= [ (2.0 * fromIntegral x / fromIntegral width - 1.0, 
                                                         2.0 * fromIntegral y / fromIntegral height - 1.0, 0.0) | y <- [0..height], x <- [0..width] ]
  
cellUpdate :: Grid -> Int -> Int -> Int
cellUpdate grid x y = if alive && ( neighborCount < 2 || neighborCount > 3 ) then 0
                      else if not alive && neighborCount == 3                then 1
                      else value
                      where value = gridAt grid x y
                            alive = (value == 1)
                            neighborCount = cellNeighbors grid x y

gridUpdate :: Grid -> Grid
gridUpdate grid = Grid (gridWidth grid) (gridHeight grid) [ cellUpdate grid x y | y <- [0..(gridHeight grid - 1)], x <- [0..(gridWidth grid - 1)] ]

gridInit :: Int -> Int -> Grid
gridInit x y = Grid x y (randomList (x * y)) 
               where randomList n = map (\x -> if (x::Int) > 5 then 1 else 0) [ fst( randomR (0,10) (mkStdGen (x*4))) |  x <- [0..(n-1)] ]

main :: IO ()
main = do
  (progName, args) <- getArgsAndInitialize
  grid <- newIORef (gridInit 50 50)  
  window <- createWindow "Game of Life"
  displayCallback $= display grid
  keyboardMouseCallback $= Just (keyboardMouse grid)
  mainLoop

display :: IORef Grid -> DisplayCallback
display grid = do 
  g <- get grid
  clear [ColorBuffer]  
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (gridPoints g)
  flush
  
keyboardMouse :: IORef Grid -> KeyboardMouseCallback
keyboardMouse grid _ Down _ _ = do 
  g <- get grid
  grid $= gridUpdate g
  postRedisplay Nothing
keyboardMouse _ _ Up _ _ = return ()
