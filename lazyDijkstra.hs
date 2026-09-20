import Data.List (foldl') -- We use foldl' for strict evaluation of the queue to prevent memory leaks
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Heap as H
import Data.Heap (MinPrioHeap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)


newtype Graph = Graph
    { edges :: HashMap String [(String, Int)] }

data Distance a = Dist a | Infinity --a is een generic. ook zorgt dist a er voor dat deze type of een distance van a krijgt of oneindig
    deriving (Show, Eq) --adding de default functions. dit stukje zorgt er voor dat er boiler plate code komt voor dit data type

instance (Ord a) => Ord (Distance a) where -- dit maakt de regels voor de funtie. dit zorgt ervoor dat de generic wel orderable moet zijn. ORD type class. ik heb dit hand matig geschreven omdat dit me meer controle geeft. als ik dat naemlijk had gedaan met deriving. Als iemand het dan aan past dan zou de order niet meer kloppen op deze manier kan dat niet gebeuren.
    Infinity <= Infinity = True -- Infinity is altijd gelijk aan Infinity 
    Infinity <= Dist x = False -- Infinity is nooit gelijk of kleiner dan een defined nummer
    Dist x <= Infinity = True -- een defined nummer is altijd kleiner dan Infinity
    Dist x <= Dist y = x <= y -- 2 waardes worden met elkaar vergeleken, ze worden gestripped van dist x en dist y naar x en y. 

addDistance :: (Num a) => Distance a -> Distance a -> Distance a -- forced dat distance a en distance a  altijd een nummer zijn. dit is zodat we de berekening wel kunnen doen. kan dit niet wordt er altijd Infinity gereturnd.
addDistance (Dist x ) (Dist y ) =  Dist (x + y)
addDistance _ _ = Infinity

    
-- Returns een lazy list of nodes in exact order van kortste aftstand
lazyDijkstra :: Graph -> String -> [(String, Distance Int)]
lazyDijkstra graph src = explore HS.empty initialQueue
  where
    initialQueue = H.fromList [(Dist 0, src)]
    
    explore :: HashSet String -> MinPrioHeap (Distance Int) String -> [(String, Distance Int)]
    explore visited queue = case H.view queue of
        Nothing -> []  
        Just ((currentDist, node), q1) ->
            if HS.member node visited
            then explore visited q1  -- Skip als al bezocht
            else 
                (node, currentDist) : explore newVisited newQueue
              where
                newVisited = HS.insert node visited
                allNeighbors = fromMaybe [] (HM.lookup node (edges graph))
                unvisitedNeighbors = filter (\(n, _) -> not (HS.member n newVisited)) allNeighbors
                newQueue = foldl' (\q (neighbor, cost) -> 
                    H.insert (addDistance currentDist (Dist cost), neighbor) q
                    ) q1 unvisitedNeighbors

findShortestDistance :: Graph -> String -> String -> Distance Int
findShortestDistance graph src dest = 
    let lazyPaths = lazyDijkstra graph src
    in fromMaybe Infinity (lookup dest lazyPaths)


graph1 :: Graph
graph1 = Graph $ HM.fromList
  [ ("A", [("B", 4), ("C", 2)])
  , ("B", [("D", 5), ("E", 3)])
  , ("C", [("B", 1), ("D", 1), ("F", 8)])
  , ("D", [("E", 1), ("G", 4), ("H", 5)])
  , ("E", [("H", 2), ("I", 8)])
  , ("F", [("G", 2), ("I", 10)])
  , ("G", [("H", 1), ("J", 6)])
  , ("H", [("I", 2), ("J", 3)])
  , ("I", [("J", 2)])
  , ("J", [])
  ] --output zou 9 moeten zijn

graph2 :: Graph
graph2 = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ] --output zou 40 moeten zijn