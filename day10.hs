type Position = (Int, Int)
type Velocity = (Int, Int)
data Point = Point Position Velocity deriving (Show)

parseInput :: String -> [Point]
parseInput = (map toPoint) . lines
  where toPoint line = Point (0, 0) (0, 0)

-- Bailing on haskell this time

exampleInput = "position=<-40181, -50237> velocity=< 4,  5>\
\position=<-40122,  30405> velocity=< 4, -3>\
\position=<-40158, -50246> velocity=< 4,  5>"

main = do
  input <- readFile "input/day10"
  putStrLn $ show $ parseInput input
