import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C
import Data.Ord
import Data.List (minimumBy)
import System.Environment (getArgs)

data Options = Options { trimStringsBy :: Int
                       , maximumDistance :: Int
                       , distanceFunction :: (C.ByteString -> C.ByteString -> Int)
                       , inputFile :: String
                       }

defaultOptions = Options { trimStringsBy = 0
                         , maximumDistance = 3
                         , distanceFunction = dist2
                         }

naiveCluster :: Int -> (C.ByteString -> C.ByteString -> Int) -> [C.ByteString] -> [(C.ByteString, [C.ByteString])]
naiveCluster maxDist dist xs = M.toList $ go xs M.empty
  where
    go :: [C.ByteString] -> Clusters C.ByteString -> Clusters C.ByteString
    go [] cs = cs
    go (x:xs) cs
      | dist x nearest <= maxDist = go xs (M.insert nearest (x : nearestNeighs) cs)
      | otherwise = go xs (M.insert x [] cs)
      where
        nearest :: C.ByteString
        nearest = bestCluster x dist cs

        nearestNeighs :: [C.ByteString]
        nearestNeighs = maybe [] id (M.lookup nearest cs)


dist2 x y = go (C.words x) (C.words y)
 where
   go [] ys = length ys
   go xs [] = length xs
   go (x:xs) (y:ys) = (if x == y then 0 else 1) + go xs ys

dist3 x y = dist2 (C.drop 87 x) (C.drop 87 y)





type Clusters a = M.Map a [a]

bestCluster :: a -> (a -> a -> Int) -> Clusters a -> a
bestCluster candidate distanceFrom clusters
 | M.null clusters = candidate
 | otherwise = minimumBy (comparing (distanceFrom candidate)) (M.keys clusters)


main = do
  (file:maxDist':_) <- getArgs
  let maxDist = read maxDist'
  fileContents <- C.readFile file
  mapM_ (C.putStrLn . pp) . (naiveCluster maxDist dist3)  . C.lines $ fileContents

pp :: (C.ByteString, [C.ByteString]) -> C.ByteString
pp (cand, neighs) = C.unlines $ (C.concat [cand, (C.pack " "), (C.pack . show $ length neighs)]) :  (map (\x -> C.append (C.pack "  ") x) neighs)

ppEntropy :: (C.ByteString, [C.ByteString]) -> C.ByteString
ppEntropy (cand, neighs) = C.unlines $ map C.unwords [words', entropies']
  where
    (words, entropies) = unzip $ clusterEntropy (cand : neighs)
    (words', entropies') = unzip $ zipWith padToEqual words entropies

    -- Pad x's && y's representation to be the same length
    padToEqual :: C.ByteString -> Double -> (C.ByteString, C.ByteString)
    padToEqual x y = (C.append x (C.replicate (-1*difference) ' '), C.append y' (C.replicate difference ' '))
      where
        y' = C.pack . take 5 . show $ y
        difference = C.length x - C.length y'

-- xs is a list of log lines
clusterEntropy :: [C.ByteString] -> [(C.ByteString, Double)]
clusterEntropy xs = map (head &&& listEntropy) asWords
  where
    asWords = transpose $ map C.words xs

listEntropy :: (Eq a) => [a] -> Double
listEntropy xs = let (_, _, e) = listEntropy' xs in e

listEntropy' :: (Eq a) => [a] -> ([Double], Double, Double)
listEntropy' [] = ([], 0, 0)
listEntropy' xs = (map px xs, qx, entropy)
  where
    entropy = sum $ map singleEntropy xs
    singleEntropy x = ((px x) * log2 ((px x) / qx))

    px x = (count x xs) / genericLength xs
    qx = 1 / genericLength (nub xs) -- if xs were uniformly distributed

    log2 n = log n / log 2
    count x xs = genericLength $ filter (==x) xs
