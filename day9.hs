import           Data.List       (maximumBy)
import qualified Data.Map.Strict as Map

type Marble = Int
type PlayerName = String
type PlayerScore = Int
data Player = Player PlayerScore deriving (Show, Eq)
getPlayerScore (Player s) = s
data MarbleGame =
  MarbleGame
  [Marble]
  (Map.Map PlayerName Player)
  deriving (Show)

addMarble :: Marble -> PlayerName -> MarbleGame -> MarbleGame
addMarble m =
  if mod m 23 == 0 then add23Marble m else addRegularMarble m

-- `rotate 2` - rotate 2 to the right
-- `rotate -7` - rotate 7 to the left
-- https://stackoverflow.com/a/44309145
rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where lxs = length xs

addRegularMarble :: Marble -> PlayerName -> MarbleGame -> MarbleGame
addRegularMarble newMarble _ (MarbleGame marbles players) =
  MarbleGame newMarbles players
    where
      newMarbles = newMarble : rotate 2 marbles

add23Marble :: Marble -> PlayerName -> MarbleGame -> MarbleGame
add23Marble newMarble playerAddingMarble (MarbleGame marbles players) =
  MarbleGame newMarbles newPlayers
    where
      rotatedMarbles = rotate (-7) marbles
      newMarbles = tail $ rotatedMarbles
      newPlayers = Map.insert playerAddingMarble newPlayer players
      newPlayer = Player (head rotatedMarbles + newMarble + getPlayerScore(fromJust $ Map.lookup playerAddingMarble players))
      fromJust (Just a) = a
      fromJust Nothing  = undefined

-- Initial state of the game
createMarbleGameWithPlayers :: Int -> MarbleGame
createMarbleGameWithPlayers n =
  MarbleGame [0] (foldl addNewPlayer Map.empty (take n [1..]))
    where addNewPlayer :: (Map.Map PlayerName Player) -> Int -> (Map.Map PlayerName Player)
          addNewPlayer map i = Map.insert key value map
            where key = "Player " ++ show i
                  value = Player 0

getNthMarbleGame :: MarbleGame -> Int -> MarbleGame
getNthMarbleGame initialMarbleGame n = allMarbleGames initialMarbleGame !! n

allMarbleGames :: MarbleGame -> [MarbleGame]
allMarbleGames initialMarbleGame = fmap snd $ iterate addNextMarble (0, initialMarbleGame)
  where addNextMarble :: (Marble, MarbleGame) -> (Marble, MarbleGame)
        addNextMarble acc@(lastMarbleAdded, currentMarbleGame) =
          (
            lastMarbleAdded + 1,
            addMarble (lastMarbleAdded + 1) (getNextPlayerName acc) currentMarbleGame
          )
        -- On the first marble, it's "Player 1" (players !! 0)
        -- On the second marble, it's "Player 2" (players !! 1),
        -- etc
        getNextPlayerName :: (Marble, MarbleGame) -> PlayerName
        getNextPlayerName (lastMarbleAdded, MarbleGame _ players) =
          "Player " ++ show (lastMarbleAdded `mod` (length players) + 1)

getHighScore :: MarbleGame -> Int
getHighScore (MarbleGame _ players) = maximum $
  Map.map (\(Player playerScore) -> playerScore) players

-- 418, 71339
-- pt 2: 418, 7133900
main = do
  let marbleGame = createMarbleGameWithPlayers 418
  -- putStrLn $ show $ getHighScore $ getNthMarbleGame marbleGame 71339
  putStrLn $ show $ getNthMarbleGame marbleGame 71339
  --let marbleGame = createMarbleGameWithPlayers 30
  --putStrLn $ show $ getHighScore $ getNthMarbleGame marbleGame 5807
