import Debug.Trace
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

data DijkstraState = DijkstraState
    { 
        vistitedSet :: HashSet String, -- zorgt ervoor dat we niet terug kunnen komen op nodes waar we al zijn geweest omdat hashsets alleen maar unique elemente toe laat.
        distanceMap :: HashMap String (Distance Int),   
        nodeQueue :: MinPrioHeap (Distance Int) String
    }

instance (Ord a) => Ord (Distance a) where -- dit maakt de regels voor de funtie. dit zorgt ervoor dat de generic wel orderable moet zijn. ORD type class. ik heb dit hand matig geschreven omdat dit me meer controle geeft. als ik dat naemlijk had gedaan met deriving. Als iemand het dan aan past dan zou de order niet meer kloppen op deze manier kan dat niet gebeuren.
    Infinity <= Infinity = True -- Infinity is altijd gelijk aan Infinity 
    Infinity <= Dist x = False -- Infinity is nooit gelijk of kleiner dan een defined nummer
    Dist x <= Infinity = True -- een defined nummer is altijd kleiner dan Infinity
    Dist x <= Dist y = x <= y -- 2 waardes worden met elkaar vergeleken, ze worden gestripped van dist x en dist y naar x en y. 

addDistance :: (Num a) => Distance a -> Distance a -> Distance a -- forced dat distance a en distance a  altijd een nummer zijn. dit is zodat we de berekening wel kunnen doen. kan dit niet wordt er altijd Infinity gereturnd.
addDistance (Dist x ) (Dist y ) =  Dist (x + y)
addDistance _ _ = Infinity


(!??) :: (Hashable k , Eq k ) => HashMap k (Distance d) -> k -> Distance d -- forceed dat k hashable is en dat k ook verglijkbaar moet zijn. dit zorgt ervoor dat we een bepaald patroon behouden. returned een distance (custom data type hier boven)
(!??) distanceMap key = fromMaybe Infinity(HM.lookup key distanceMap) -- kijkt in de hashmap of er een instance is van een distance met de key (k) zo ja returned dit dat getal (distance d). zo niet returned het Infinity. dit mag omdat we boven aan de data type hebben gemaakt en dit is 1 van de toegestaande waardes.


findShortestDistance :: Graph -> String -> String -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest 
    where 
        initialVisited = HS.empty -- lege hashset nodes
        initialDistances = HM.singleton src (Dist 0) -- maakt een hashmap met dist 0 voor de init node
        initialQueue = H.fromList [(Dist 0, src)] -- maakt een min heap met de init node en dist 0
        initialState = DijkstraState initialVisited initialDistances initialQueue -- maakt een dijkstra stade met de waardes
    
        processQueue :: DijkstraState -> HashMap String (Distance Int)
        processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of 
            Nothing -> d0
            Just ((minDist, node), q1) ->
                if node == dest then d0 -- als we bij de destination node zijn returnen we de distance als map.
                else if HS.member node v0 then processQueue (ds {nodeQueue = q1}) -- Skip als al bezocht
                else
                    let v1 = HS.insert node v0 -- voegt de current node toe aan de visted set.
                        allNeighbors = fromMaybe [] (HM.lookup node (edges graph)) --pakt alle neighbors van de node die we nu aan het bekijken zijn. dit is een list van tuples. (node, cost)
                        unvisitedNeighbors = filter (\(n, _) -> not (HS.member n v1)) allNeighbors -- filtererd uit de nodes die we al hebben bezocht. zodat we niet terug gaan naar een node waar we al zijn geweest.
                    in processQueue $ foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors -- calls function recursively until the queue is empty or the destination node is reached. It updates the state with new distances and visited nodes.

     
        foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
            let altDistance = addDistance (d0 !?? current) (Dist cost) -- calculeert de afstanden van de current node naar de neightbour node
            in if altDistance < d0 !?? neighborNode -- als die kleiner is update
               then DijkstraState v1 (HM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1) 
               else ds -- als die niet kleiner is return de state zoals die is. 


graph1 :: Graph
graph1 = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]