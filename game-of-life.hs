import Graphics.UI.GLUT
import System.Random
import Data.IORef
import Control.Concurrent

--main
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  grid <- newIORef (gridInit 50 50)
  
  displayCallback $= display grid
  --idleCallback $= Just (idle grid)
  keyboardMouseCallback $= Just (keyboardMouse grid)
  mainLoop

--display callback
display :: IORef Grid -> DisplayCallback
display grid = do 
  clear [ColorBuffer]
  g <- get grid
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (gridPoints g)
  flush
  
--idle callback (not used)
idle :: IORef Grid -> IdleCallback
idle grid = do
  a <- get grid
  grid $= gridUpdate a
  --wait 200000
  threadDelay (16000)
  postRedisplay Nothing
  
--Keyboard and mouse callback
keyboardMouse :: IORef Grid -> KeyboardMouseCallback
keyboardMouse grid _ Down _ _ = do 
  a <- get grid
  grid $= gridUpdate a
  postRedisplay Nothing
  
keyboardMouse _ _ Up _ _ = return ()
keyboard _ _ _ _ _ = return ()
  

data Grid = Grid Int Int [Int] deriving (Show)

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


gridNeighbors :: Grid -> Int -> Int -> Int
gridNeighbors grid x y = (gridAt grid (x-1) (y+1)) + (gridAt grid x (y+1)) + (gridAt grid (x+1) (y+1)) +
                         (gridAt grid (x-1) y)     +                         (gridAt grid (x+1) y)   +
                         (gridAt grid (x-1) (y-1)) + (gridAt grid x (y-1)) + (gridAt grid (x+1) (y-1))
          
generatePoints :: Int -> Int -> [(GLfloat,GLfloat,GLfloat)]
generatePoints width height= [ (2.0 * fromIntegral x/fromIntegral width - 1.0, 2.0 * fromIntegral y/fromIntegral height - 1.0, 0.0) | y <- [0..height], x <- [0..width] ]

gridPoints :: Grid -> [(GLfloat,GLfloat,GLfloat)]
gridPoints grid = (map fst ( filter isLive (zip (generatePoints (gridWidth grid -1) (gridHeight grid - 1) ) (gridData grid) ) )) 
                  where isLive x = (snd x) > 0

  
isAlive :: Grid -> Int -> Int -> Int
isAlive grid x y = if (currentlyAlive && ( neighbourCount < 2 || neighbourCount > 3 )) then 0
                   else if ((not currentlyAlive) && (neighbourCount == 3))            then 1
                   else currentValue
                   where currentValue = gridAt grid x y
                         currentlyAlive =  currentValue == 1
                         neighbourCount = gridNeighbors grid x y


gridUpdate :: Grid -> Grid
gridUpdate grid = Grid (gridWidth grid) (gridHeight grid) [ isAlive grid x y | y <- [0..(gridHeight grid - 1)], x <- [0..(gridWidth grid - 1)] ]


greaterThan5 :: Int -> Int
greaterThan5 x = if x > 5 then 1 else 0
randomGrid :: Int -> [Int]
randomGrid n = map greaterThan5 [ fst( randomR (0,10) (mkStdGen (x*4))) |  x <- [0..(n-1)] ]

gridInit :: Int -> Int -> Grid
gridInit x y = Grid x y (randomGrid (x * y))