import           Data.List.Split

type Child = Tree
type Metadata = Int
data Tree = Leaf [Metadata] | Tree [Child] [Metadata]
  deriving (Show)

process :: String -> Int
process = sumMetadata . convertToTree . toInts

toInts :: String -> [Int]
toInts = (map read) . (splitOn " ")

convertToTree :: [Int] -> Tree
convertToTree = fst . convertToTreeAndRest

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

sumMetadata :: Tree -> Int
sumMetadata (Leaf metadata) = sum metadata
sumMetadata (Tree children metadata) = sum metadata + sum (map sumMetadata children)

main = do
  input <- readFile "input/day8"
  putStrLn $ show(process input)

exampleString = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
