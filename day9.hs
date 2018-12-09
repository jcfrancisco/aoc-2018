import           Data.List

type CurrentMarbleIndex = Int
type Marble = Int
type PlayerName = String
data Player = Player PlayerName [Marble] deriving (Show, Eq)
data MarbleGame =
  MarbleGame
  CurrentMarbleIndex
  [Marble]
  [Player]
  deriving (Show)

addMarble :: Marble -> Player -> MarbleGame -> MarbleGame
addMarble m =
  if mod m 23 == 0 then add23Marble m else addRegularMarble m

addRegularMarble :: Marble -> Player -> MarbleGame -> MarbleGame
addRegularMarble newMarble _ (MarbleGame currentMarbleIndex marbles players) =
  MarbleGame newMarbleIndexAdjusted newMarbles players
    where newMarbleIndexAdjusted =
            if newMarbleIndex == 0
            then length marbles
            else newMarbleIndex
          newMarbleIndex = mod (currentMarbleIndex + 2) (length marbles)
          newMarbles =
            (take newMarbleIndexAdjusted marbles)
            ++ (newMarble : (drop newMarbleIndexAdjusted marbles))

add23Marble :: Marble -> Player -> MarbleGame -> MarbleGame
add23Marble newMarble playerAddingMarble (MarbleGame currentMarbleIndex marbles players) =
  MarbleGame newMarbleIndex newMarbles newPlayers
    where newMarbleIndex =
            if sevenCounterClockwise < 0
            then (length marbles) - (abs sevenCounterClockwise)
            else sevenCounterClockwise
          sevenCounterClockwise = currentMarbleIndex - 7
          newMarbles =
            (take newMarbleIndex marbles)
            ++ (drop (newMarbleIndex + 1) marbles)
          newPlayers =
            map
            (\player@(Player playerName playerMarbles) ->
              if player == playerAddingMarble
              then Player playerName (marbles !! newMarbleIndex : newMarble : playerMarbles)
              else player
            )
            players

-- Initial state of the game
createMarbleGameWithPlayers :: Int -> MarbleGame
createMarbleGameWithPlayers n =
  MarbleGame 0 [0] (take n $ map createNewPlayer [1..])
    where createNewPlayer :: Int -> Player
          createNewPlayer i = Player ("Player " ++ show i) []

getNthMarbleGame :: MarbleGame -> Int -> MarbleGame
getNthMarbleGame initialMarbleGame n = allMarbleGames initialMarbleGame !! n

-- (0, initialMarbleGame)
-- (1, addMarble 1 initialMarbleGame)
-- (2, addMarble 2 (addMarble 1 initialMarbleGame))
-- etc
allMarbleGames :: MarbleGame -> [MarbleGame]
allMarbleGames initialMarbleGame = map snd $ iterate addNextMarble (0, initialMarbleGame)
  where addNextMarble :: (Marble, MarbleGame) -> (Marble, MarbleGame)
        addNextMarble acc@(lastMarbleAdded, currentMarbleGame) =
          (
            lastMarbleAdded + 1,
            addMarble (lastMarbleAdded + 1) (getNextPlayer acc) currentMarbleGame
          )
        -- On the first marble, it's "Player 1" (players !! 0)
        -- On the second marble, it's "Player 2" (players !! 1),
        -- etc
        getNextPlayer :: (Marble, MarbleGame) -> Player
        getNextPlayer (lastMarbleAdded, (MarbleGame _ _ players)) =
          players !! mod lastMarbleAdded (length players)

getHighScore :: MarbleGame -> Int
getHighScore (MarbleGame _ _ players) =
  maximum $ map (\(Player _ marbles) -> sum marbles) players

-- 418, 71339
-- pt 2: 418, 7133900
main = do
  let marbleGame = createMarbleGameWithPlayers 418
  putStrLn $ show $ getHighScore $ getNthMarbleGame marbleGame 71339
