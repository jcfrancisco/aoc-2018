import           Data.List.Split

-------------------------------------
-- Parsing the string into a Tree  --
-------------------------------------
type Child = Tree
type Metadata = Int
data Tree = Leaf [Metadata] | Tree [Child] [Metadata]
  deriving (Show)

convertToTree :: [Int] -> Tree
convertToTree = fst . convertToTreeAndRest

-- Given a list of integers, parse the next tree
-- in that list of integers. Return a tuple,
-- where the first value is the tree and the second
-- is the rest of the unparsed list.
--
-- TODO: Learn the more Haskell-y idioms which to write parsers
-- like this, lol.
convertToTreeAndRest :: [Int] -> (Tree, [Int])
convertToTreeAndRest ints =
  case childCount of
    0 ->
      (
        Leaf (take metadataCount (drop 2 ints)),
        (drop (metadataCount + 2) ints)
      )
    n ->
      (
        Tree children (take metadataCount rest),
        (drop metadataCount rest)
      )
  where childCount = head ints
        metadataCount = ints !! 1
        childrenAndRest = last $
          take
          (childCount + 1)
          (iterate appendNextChild ([], drop 2 ints))
        children = fst childrenAndRest
        rest = snd childrenAndRest

appendNextChild :: ([Tree], [Int]) -> ([Tree], [Int])
appendNextChild (childrenSoFar, ints) =
  (childrenSoFar ++ [fst nextTreeAndRest], snd nextTreeAndRest)
    where nextTreeAndRest = convertToTreeAndRest ints

-------------------------------------
-- Actual Sum and Value algorithms --
-------------------------------------

sumMetadata :: Tree -> Int
sumMetadata (Leaf metadata) = sum metadata
sumMetadata (Tree children metadata) = sum metadata + sum (map sumMetadata children)

findValue :: Tree -> Int
findValue (Leaf metadata)          = sum metadata
findValue (Tree children metadata) = sum $ map valueForMetadataIndex metadata
  where valueForMetadataIndex :: Int -> Int
        valueForMetadataIndex metadataIndex =
          if length children < metadataIndex
          then 0
          else findValue(children !! (metadataIndex - 1))

-- Takes in the input string of the problem and returns
-- a tuple. First item in the tuple is the answer to the
-- first part of the problem, second item is the answer
-- to the second part of the problem ("value").
process :: String -> (Int, Int)
process s = (sumMetadata tree, findValue tree)
  where tree = (convertToTree . toInts) s

toInts :: String -> [Int]
toInts = (map read) . (splitOn " ")

main = do
  input <- readFile "input/day8"
  let (sum, value) = process input
  putStrLn $ "Sum is " ++ show sum ++ ". Value is " ++ show value
