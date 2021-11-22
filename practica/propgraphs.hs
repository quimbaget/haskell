-- TYPE SYNONYMS

type Label = String

type Id = String

-- CUSTOM DATA TYPES

data Prop = Prop Label String deriving (Show)

data Node = Node Id Label [Prop] deriving (Show)

data Edge = Edge Id Id Id Label [Prop] deriving (Show) -- Edge Id, origin node ID, destination node ID, list of props

data PG = PG [Node] [Edge] deriving (Show)

-- POPERTY GRAPH (PG) INSTANCES

{- instance Show Prop where
  show (Prop l s) = "(" ++ show l ++ "," ++ show s ++ ")"

instance Show Node where
  show (Node id l props) = show id ++ "[" ++ show l ++ "]" ++ concatMap show props

instance Show Edge where
  show (Edge id n1 n2 label props) = "(" ++ show n1 ++ ") - " ++ show id ++ "[" ++ label ++ "] -> (" ++ show n2 ++ ")" ++ concatMap show props -}

{- instance Show PG where
  show (PG nodes edges) = (map show nodes) ++ (map show edges) -}

-- I/O FUNCTIONS

generateEdge :: [String] -> Edge
generateEdge (e : (originNode : nodes)) = Edge e originNode (last nodes) "" []

generateNode :: [String] -> Node
generateNode (id : label) = Node id (last label) [] -- when def label works, we can generate node without label here

generateLabel :: [String] -> Label
generateLabel (id : label) = last label

generateProp :: String -> String -> Prop
generateProp = Prop

parseLine :: String -> [String]
parseLine = words

parseFile :: String -> [String]
parseFile = lines

parseRhoFile :: String -> [Edge]
parseRhoFile file = map (generateEdge . parseLine) (parseFile file)

filterNodes :: [Edge] -> [String] -> [String]
filterNodes edges = filter (\x -> not $ edgeIsInside (head (parseLine x)) edges)

filterLabels :: [Edge] -> [String] -> [String]
filterLabels edges = filter (\x -> edgeIsInside (head (parseLine x)) edges)

parseLamdaFileIntoNodes :: String -> [Edge] -> [Node]
parseLamdaFileIntoNodes file edges = map (generateNode . parseLine) (filterNodes edges (parseFile file))

parseLambdaFileIntoLabels :: String -> [Edge] -> [Label]
parseLambdaFileIntoLabels file edges = map (generateLabel . parseLine) (filterNodes edges (parseFile file))

-- PROPERTY GRAPHS METHODS

edgeIsEqual :: String -> Edge -> Bool
edgeIsEqual edge (Edge id _ _ _ _) = edge == id

edgeIsInside :: String -> [Edge] -> Bool
edgeIsInside _ [] = False
edgeIsInside edge (e : es) = edgeIsInside edge es || edgeIsEqual edge e

hasEdge :: PG -> String -> Bool
hasEdge (PG _ edges) edge = edgeIsInside edge edges

setEdgeLabel :: Edge -> Label -> Maybe Edge
setEdgeLabel (Edge e n1 n2 lab props) l
  | not (null lab) = Nothing
  | otherwise = Just $ Edge e n1 n2 l props

setNodeLabel :: Node -> Label -> Maybe Node
setNodeLabel (Node n lab props) l
  | not (null lab) = Nothing
  | otherwise = Just $ Node n l props

{- defVlabel :: PG -> Node -> Label -> PG
defVlabel pg node label =  -}

addEdge :: PG -> String -> String -> String -> PG
addEdge (PG nodes edges) e n1 n2 = PG nodes (Edge e n1 n2 "" [] : edges)

populate :: String -> String -> String -> String -> PG
populate rho lamda sigma props =
  let edges = parseRhoFile rho
      nodes = parseLamdaFileIntoNodes lamda edges
   in PG nodes edges

{-
    defVprop :: PG -> Node -> Prop -> PG
    defEProp :: PG -> Edge -> Prop -> PG
    defVlabel :: PG -> Node -> Prop -> PG
    defElabel :: PG -> Edge -> Prop -> PG
    showGraph :: PG
-}

main :: IO ()
main = do
  putStrLn "Input rhoFilePath"
  path <- getLine
  rhoFile <- readFile path

  putStrLn "Input lamdaFile"
  path <- getLine
  lamdaFile <- readFile path

  {-   putStrLn "Input sigmaFile"
    path <- getLine
    sigmaFile <- readFile path

    putStrLn "Input propFile"
    path <- getLine
    propFile <- readFile path -}

  let pg = populate rhoFile lamdaFile "sigmaFile" "propFile"
  print pg