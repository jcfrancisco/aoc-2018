type CurrentMarbleIndex = Int
type Marble = Int
data MarbleGame =
  MarbleGame
  CurrentMarbleIndex
  [Marble]
  deriving (Show)

addMarble :: Marble -> MarbleGame -> MarbleGame
addMarble m mg =
  if mod m 23 == 0 then add23Marble m mg else addRegularMarble m mg

addRegularMarble :: Marble -> MarbleGame -> MarbleGame
addRegularMarble newMarble (MarbleGame currentMarbleIndex marbles) =
  MarbleGame newMarbleIndexAdjusted newMarbles
    where newMarbleIndexAdjusted =
            if newMarbleIndex == 0
            then length marbles
            else newMarbleIndex
          newMarbleIndex = mod (currentMarbleIndex + 2) (length marbles)
          newMarbles =
            (take newMarbleIndexAdjusted marbles)
            ++ (newMarble : (drop newMarbleIndexAdjusted marbles))

add23Marble :: Marble -> MarbleGame -> MarbleGame
add23Marble m mg = mg

marbles = 418
lastMarble = 71339
-- Initial state of the game
marbleGame = MarbleGame 0 [0]

getNthMarbleGame :: MarbleGame -> Int -> MarbleGame
getNthMarbleGame initialMarbleGame n =
  foldl
  (\currentMarbleGame addNthMarble -> addNthMarble currentMarbleGame)
  initialMarbleGame
  (take n $ map addMarble [1..])

