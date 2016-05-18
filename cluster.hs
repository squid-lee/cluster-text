import qualified Data.Map as M
import Data.Ord
import Data.List (minimumBy)
import System.Environment (getArgs)

naiveCluster :: Int -> [String] -> [(String, [String])]
naiveCluster maxDist xs = M.toList $ go xs M.empty
  where
    go :: [String] -> Clusters String -> Clusters String
    go [] cs = cs
    go (x:xs) cs
      | dist3 x best <= maxDist = go xs (M.insert best (x : bestValues) cs)
      | otherwise = go xs (M.insert x [] cs)
      where
        best = bestCluster x dist3 cs
        bestValues = maybe [] id (M.lookup best cs)


dist2 x y = go (words x) (words y)
 where
   go [] ys = length ys
   go xs [] = length xs
   go (x:xs) (y:ys) = (if x == y then 0 else 1) + go xs ys

dist3 x y = dist2 (drop 87 x) (drop 87 y)





type Clusters a = M.Map a [a]

bestCluster :: (Ord dist, Num dist) => a -> (a -> a -> dist) -> Clusters a -> a
bestCluster candidate distanceFrom clusters
 | M.null clusters = candidate
 | otherwise = minimumBy (comparing (distanceFrom candidate)) (M.keys clusters)


main = do
  (file:maxDist':_) <- getArgs
  let maxDist = read maxDist'
  fileContents <- readFile file
  mapM_ (putStrLn . pp) . (naiveCluster maxDist)  . lines $ fileContents

pp :: (String, [String]) -> String
pp (cand, neighs) = unlines $ (cand ++ " " ++ (show $ length neighs)) : (map ("  " ++) neighs)