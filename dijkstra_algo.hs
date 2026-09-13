import Debug.Trace
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
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


lookupTable :: (Hashable k , Eq k ) => HashMap k (Distance d) -> k -> Distance d -- forceed dat k hashable is en dat k ook verglijkbaar moet zijn. dit zorgt ervoor dat we een bepaald patroon behouden. returned een distance (custom data type hier boven)
lookupTable distanceMap key = fromMaybe Infinity(Hm.lookup key distanceMap) -- kijkt in de hashmap of er een instance is van een distance met de key (k) zo ja returned dit dat getal (distance d). zo niet returned het Infinity. dit mag omdat we boven aan de data type hebben gemaakt en dit is 1 van de toegestaande waardes.


findShortestDistance :: Graph -> String -> String -> Distance Int -- neemt een de graph, de start node (string) en destiantion node (string) en berekend de distance. de kleinste wordt gereturned in als distance



graph1 :: Graph
graph1 = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]