import           Data.List
import qualified Data.Sequence as S

type CurrentMarbleIndex = Int
type Marble = Int
type PlayerName = String
type PlayerScore = Int
data Player = Player PlayerName PlayerScore deriving (Show)
instance Eq Player where
  (Player name1 _) == (Player name2 _) = name1 == name2
data MarbleGame =
  MarbleGame
  CurrentMarbleIndex
  (S.Seq Marble)
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
          newMarbles = S.insertAt newMarbleIndexAdjusted newMarble marbles

add23Marble :: Marble -> Player -> MarbleGame -> MarbleGame
add23Marble newMarble playerAddingMarble (MarbleGame currentMarbleIndex marbles players) =
  MarbleGame newMarbleIndex newMarbles newPlayers
    where newMarbleIndex =
            if sevenCounterClockwise < 0
            then (length marbles) - (abs sevenCounterClockwise)
            else sevenCounterClockwise
          sevenCounterClockwise = currentMarbleIndex - 7
          newMarbles = S.deleteAt newMarbleIndex marbles
          newPlayers =
            map
            (\player@(Player playerName playerScore) ->
              -- TODO: Optimize Eq for Player
              if player == playerAddingMarble
              then Player playerName ((fromJust (S.lookup newMarbleIndex marbles)) + newMarble + playerScore)
              else player
            )
            players
          fromJust (Just a) = a
          fromJust Nothing  = undefined

-- Initial state of the game
createMarbleGameWithPlayers :: Int -> MarbleGame
createMarbleGameWithPlayers n =
  MarbleGame 0 (S.singleton 0) (take n $ map createNewPlayer [1..])
    where createNewPlayer :: Int -> Player
          createNewPlayer i = Player ("Player " ++ show i) 0

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
        getNextPlayer :: (Marble, MarbleGame) -> Player
        getNextPlayer (lastMarbleAdded, (MarbleGame _ _ players)) =
          players !! mod lastMarbleAdded (length players)
          --Player ("Player " ++ show(1 + (mod lastMarbleAdded (length players)))) 0

getHighScore :: MarbleGame -> Int
getHighScore (MarbleGame _ _ players) =
  getScore $ maximumBy (
    \(Player _ score1) (Player _ score2) ->
      if score1 > score2 then GT
      else if score2 < score1 then LT
      else EQ
  ) players
    where getScore (Player _ s) = s

-- 418, 71339
-- pt 2: 418, 7133900
main = do
  let marbleGame = createMarbleGameWithPlayers 418
  putStrLn $ show $ getHighScore $ getNthMarbleGame marbleGame 7133900
