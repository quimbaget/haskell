-- TYPE SYNONYMS

type Label = String

type Id = String

-- CUSTOM DATA TYPES

data Prop = Prop Label String

data Node = Node Id Label [Prop]

data Edge = Edge Id Id Id Label [Prop]

data PG = PG [Node] [Edge]

-- INSTANCES

instance Show Prop where
  show (Prop l s) = "(" ++ show l ++ "," ++ show s ++ ")"

instance Show Node where
  show (Node id l props) = show id ++ "[" ++ show l ++ "]" ++ concatMap show props

instance Show Edge where
  show (Edge id n1 n2 label props) = "(" ++ show n1 ++ ") - " ++ show id ++ "[" ++ label ++ "] -> (" ++ show n2 ++ ")" ++ concatMap show props

instance Show PG where
  show (PG nodes edges) = show $ map show nodes ++ map show edges

-- I/O FUNCTIONS AND PARSERS

generateEdge :: [String] -> Edge
generateEdge (e : (originNode : nodes)) = Edge e originNode (last nodes) "" []

generateNode :: [String] -> Node
generateNode (id : label) = Node id (last label) [] -- when def label works, we can generate node without label here

generateLabel :: [String] -> Label
generateLabel (id : label) = last label

generateProp :: String -> String -> Prop
generateProp = Prop

-- Given a string which represents a line, it filters the line creating an array of string
-- which is splitted at every whitespace of the line
parseLine :: String -> [String]
parseLine = words

-- Given a filepath, creates an array of strings which represent every line of the file
parseFile :: String -> [String]
parseFile = lines

-- Given a Rho file it parses it into an array of edges without label or props
parseRhoFile :: String -> [Edge]
parseRhoFile file = map (generateEdge . parseLine) (parseFile file)

filterNodes :: [Edge] -> [String] -> [String]
filterNodes edges = filter (\x -> not $ edgeIsInside (head (parseLine x)) edges)

filterLabels :: [Edge] -> [String] -> [String]
filterLabels edges = filter (\x -> edgeIsInside (head (parseLine x)) edges)

-- Given a lambda file, it parses it into words, filters out the edges and creates nodes
-- with the resulting lines
parseLamdaFileIntoNodes :: String -> [Edge] -> [Node]
parseLamdaFileIntoNodes file edges = map (generateNode . parseLine) (filterNodes edges (parseFile file))

-- Given a lambda file, it parses it into words, filtering out the nodes and
-- creates labels with the resulting lines
parseLambdaFileIntoLabels :: String -> [Edge] -> [Label]
parseLambdaFileIntoLabels file edges = map (generateLabel . parseLine) (filterLabels edges (parseFile file))

-- PROPERTY GRAPHS METHODS

edgeHasId :: String -> Edge -> Bool
edgeHasId edge (Edge id _ _ _ _) = edge == id

nodeHasId :: String -> Node -> Bool
nodeHasId node (Node id label props) = node == id

propsAreEqual :: Prop -> Prop -> Bool
propsAreEqual (Prop l1 _) (Prop l2 _) = l1 == l2

edgeIsInside :: String -> [Edge] -> Bool
edgeIsInside _ [] = False
edgeIsInside edge (e : es) = edgeIsInside edge es || edgeHasId edge e

setEdgeLabel :: Edge -> Label -> Maybe Edge
setEdgeLabel (Edge e n1 n2 lab props) l
  | not (null lab) = Nothing
  | otherwise = Just $ Edge e n1 n2 l props

setNodeLabel :: Node -> Label -> Maybe Node
setNodeLabel (Node n lab props) l
  | not (null lab) = Nothing
  | otherwise = Just $ Node n l props

searchNodeInGraph :: PG -> Id -> Maybe Node
searchNodeInGraph (PG [] _) _ = Nothing
searchNodeInGraph (PG (n : ns) _) id
  | nodeHasId id n = Just n
  | otherwise = searchNodeInGraph (PG ns []) id

searchEdgeInGraph :: PG -> Id -> Maybe Edge
searchEdgeInGraph (PG _ []) _ = Nothing
searchEdgeInGraph (PG _ (e : es)) id
  | edgeHasId id e = Just e
  | otherwise = searchEdgeInGraph (PG [] es) id

nodesAreEqual :: Node -> Node -> Bool
nodesAreEqual (Node n1 _ _) (Node n2 _ _) = n1 == n2

edgesAreEqual :: Edge -> Edge -> Bool
edgesAreEqual (Edge e1 _ _ _ _) (Edge e2 _ _ _ _) = e1 == e2

updateProp :: Prop -> [Prop] -> [Prop]
updateProp prop [] = [prop]
updateProp prop (p : ps)
  | propsAreEqual prop p = prop : ps
  | otherwise = p : updateProp prop ps

updateEdge :: Edge -> [Edge] -> [Edge]
updateEdge e [] = [e]
updateEdge e1 (e : es)
  | edgesAreEqual e1 e = e1 : es
  | otherwise = e : updateEdge e1 es

updateNode :: Node -> [Node] -> [Node]
updateNode n [] = [n]
updateNode n1 (n : ns)
  | nodesAreEqual n1 n = n1 : ns
  | otherwise = n : updateNode n1 ns

updateNodeProps :: Node -> Prop -> Node
updateNodeProps (Node n1 l props) p = Node n1 l $ updateProp p props

updateEdgeProps :: Edge -> Prop -> Edge
updateEdgeProps (Edge e1 n1 n2 l props) p = Edge e1 n1 n2 l $ updateProp p props

defVprop :: PG -> Node -> Prop -> PG
defVprop (PG ns es) (Node n lab props) prop =
  do
    let node = searchNodeInGraph (PG ns es) n
    case node of
      Nothing -> PG ns es
      Just node -> PG (updateNode (updateNodeProps node prop) ns) es

defVlabel :: PG -> Node -> Label -> Either String PG
defVlabel (PG ns es) (Node n lab props) label =
  do
    let node = searchNodeInGraph (PG ns es) n
    case node of
      Nothing -> Left "Error"
      Just node -> do
        let newNode = setNodeLabel node lab
        case newNode of
          Just newNode -> Right (PG (updateNode newNode ns) es)
          Nothing -> Left "Error"

defEprop :: PG -> Edge -> Prop -> PG
defEprop (PG ns es) (Edge e n1 n2 lab props) prop =
  do
    let edge = searchEdgeInGraph (PG ns es) e
    case edge of
      Nothing -> PG ns es
      Just edge -> PG ns $ updateEdge (updateEdgeProps edge prop) es

defElabel :: PG -> Edge -> Label -> Either String PG
defElabel (PG ns es) (Edge e n1 n2 lab props) label =
  do
    let edge = searchEdgeInGraph (PG ns es) e
    case edge of
      Nothing -> Left "Error"
      Just edge -> do
        let newEdge = setEdgeLabel edge lab
        case newEdge of
          Just newEdge -> Right $ PG ns $ updateEdge newEdge es
          Nothing -> Left "Error"

addEdge :: PG -> String -> String -> String -> PG
addEdge (PG nodes edges) e n1 n2 = PG nodes (Edge e n1 n2 "" [] : edges)

populate :: String -> String -> String -> String -> PG
populate rho lamda sigma props =
  let edges = parseRhoFile rho
      nodes = parseLamdaFileIntoNodes lamda edges
      labels = parseLambdaFileIntoLabels lamda edges
   in PG nodes edges

-- MAIN FUNCTION

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