-- TYPE SYNONYMS

type Label = String

type Id = String

-- CUSTOM DATA TYPES

data Prop = Prop Label String deriving (Show)

data Node = Node Id Label [Prop] deriving (Show)

data Edge = Edge Id Id Id Label [Prop] deriving (Show)

data PG = PG [Node] [Edge] deriving (Show)

-- INSTANCES

-- instance Show Prop where
--   show (Prop l s) = "(" ++ show l ++ "," ++ show s ++ ")"

-- instance Show Node where
--   show (Node id l props) = show id ++ "[" ++ show l ++ "]" ++ concatMap show props

-- instance Show Edge where
--   show (Edge id n1 n2 label props) = "(" ++ show n1 ++ ") - " ++ show id ++ "[" ++ label ++ "] -> (" ++ show n2 ++ ")" ++ concatMap show props

-- instance Show PG where
--   show (PG nodes edges) = show $ map show nodes ++ map show edges

-- I/O FUNCTIONS AND PARSERS

generateEdge :: [String] -> Edge
generateEdge (e : (originNode : nodes)) = Edge e originNode (last nodes) "" []

generateNode :: [String] -> Node
generateNode (id : label) = Node id (last label) [] -- when def label works, we can generate node without label here

generateLabel :: [String] -> (Id, Label)
generateLabel (id : label) = (id, last label)

generateProp :: [String] -> (Id, Prop)
generateProp (id : (l : value)) = (id, Prop l (last value))

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

parseSigmaFileIntoNodeProps :: String -> [Edge] -> [(Id, Prop)]
parseSigmaFileIntoNodeProps file edges = map (generateProp . parseLine) (filterNodes edges (parseFile file))

parseSigmaFileIntoEdgeProps :: String -> [Edge] -> [(Id, Prop)]
parseSigmaFileIntoEdgeProps file edges = map (generateProp . parseLine) (filterLabels edges (parseFile file))

-- Given an array of edges and an array of strings each one representing a line
-- returns the lines filtering out the edges provided
filterNodes :: [Edge] -> [String] -> [String]
filterNodes edges = filter (\x -> not $ edgeIsInside (head (parseLine x)) edges)

-- Given an array of edges and an array of strings each one representing a line
-- returns the lines that contains the edges provided
filterLabels :: [Edge] -> [String] -> [String]
filterLabels edges = filter (\x -> edgeIsInside (head (parseLine x)) edges)

-- Given a lambda file, it parses it into words, filters out the edges and creates nodes
-- with the resulting lines
parseLambdaFileIntoNodes :: String -> [Edge] -> [Node]
parseLambdaFileIntoNodes file edges = map (generateNode . parseLine) (filterNodes edges (parseFile file))

-- Given a lambda file, it parses it into words, filtering out the nodes and
-- creates labels with the resulting lines
parseLambdaFileIntoLabels :: String -> [Edge] -> [(Id, Label)]
parseLambdaFileIntoLabels file edges = map (generateLabel . parseLine) (filterLabels edges (parseFile file))

-- PROPERTY GRAPHS METHODS

-- Given an ID and an Edge
-- returns true if the edge id is the same as the one provded, false otherwise
edgeHasId :: String -> Edge -> Bool
edgeHasId edge (Edge id _ _ _ _) = edge == id

-- Given an ID and a Node
-- returns true if the node id is the same as the one provded, false otherwise
nodeHasId :: String -> Node -> Bool
nodeHasId node (Node id label props) = node == id

-- Given prop1 and prop2
-- Returns TRUE if label of prop1 is the same as label of prop2
-- Reeturns FALSE otherwise
propsAreEqual :: Prop -> Prop -> Bool
propsAreEqual (Prop l1 _) (Prop l2 _) = l1 == l2

-- Given an edge id e1 and a string og edges es
-- Returns TRUE if e1 is inside es
-- Returns FALSE otherwise
edgeIsInside :: String -> [Edge] -> Bool
edgeIsInside _ [] = False
edgeIsInside edge (e : es) = edgeIsInside edge es || edgeHasId edge e

-- Given an edge an a label
-- If edge's label is empty it returns a new edge with label property updated
-- If edge's label has already a value, it returns nothing
setEdgeLabel :: Edge -> Label -> Maybe Edge
setEdgeLabel (Edge e n1 n2 lab props) l
  | not (null lab) = Nothing
  | otherwise = Just $ Edge e n1 n2 l props

-- Given a node an a label
-- If node's label is empty it returns a new node with label property updated
-- If node's label has already a value, it returns nothing
setNodeLabel :: Node -> Label -> Maybe Node
setNodeLabel (Node n lab props) l
  | not (null lab) = Nothing
  | otherwise = Just $ Node n l props

-- Given a grapgh PG and the ID of a node
-- Returns NODE if there's a node with ID in PG
-- Returns NOTHING otherwise
searchNodeInGraph :: PG -> Id -> Maybe Node
searchNodeInGraph (PG [] _) _ = Nothing
searchNodeInGraph (PG (n : ns) _) id
  | nodeHasId id n = Just n
  | otherwise = searchNodeInGraph (PG ns []) id

-- Given a grapgh PG and the ID of an edge
-- Returns EDGE if there's an edge with ID in PG
-- Returns NOTHING otherwise
searchEdgeInGraph :: PG -> Id -> Maybe Edge
searchEdgeInGraph (PG _ []) _ = Nothing
searchEdgeInGraph (PG _ (e : es)) id
  | edgeHasId id e = Just e
  | otherwise = searchEdgeInGraph (PG [] es) id

-- Given two nodes n1 and n2
-- Returns TRUE if id of n1 is equal to id of n2
-- Returns FALSE otherwise
nodesAreEqual :: Node -> Node -> Bool
nodesAreEqual (Node n1 _ _) (Node n2 _ _) = n1 == n2

-- Given two edges e1 and e2
-- Returns TRUE if id of n1 is equal to id of n2
-- Returns FALSE otherwise
edgesAreEqual :: Edge -> Edge -> Bool
edgesAreEqual (Edge e1 _ _ _ _) (Edge e2 _ _ _ _) = e1 == e2

-- Given a prop and an array of props
-- Adds the new prop to the array if there's no prop with the same label
-- Replaces the old prop from the array if there's a prop with the same label
-- Returns the resulting array of props
updateProp :: Prop -> [Prop] -> [Prop]
updateProp prop [] = [prop]
updateProp prop (p : ps)
  | propsAreEqual prop p = prop : ps
  | otherwise = p : updateProp prop ps

-- Given a prop and an array of props
-- Adds the new prop to the array if there's no prop with the same label
-- Replaces the old prop from the array if there's a prop with the same label
-- Returns the resulting array of props
updateEdge :: Edge -> [Edge] -> [Edge]
updateEdge e [] = [e]
updateEdge e1 (e : es)
  | edgesAreEqual e1 e = e1 : es
  | otherwise = e : updateEdge e1 es

-- Given a prop and an array of props
-- Adds the new prop to the array if there's no prop with the same label
-- Replaces the old prop from the array if there's a prop with the same label
-- Returns the resulting array of props
updateNode :: Node -> [Node] -> [Node]
updateNode n [] = [n]
updateNode n1 (n : ns)
  | nodesAreEqual n1 n = n1 : ns
  | otherwise = n : updateNode n1 ns

-- Given a node and a prop
-- Returns a new node with the props updated including prop
updateNodeProps :: Node -> Prop -> Node
updateNodeProps (Node n1 l props) p = Node n1 l $ updateProp p props

-- Given an edge and a prop
-- Returns a new edge with the props updated including prop
updateEdgeProps :: Edge -> Prop -> Edge
updateEdgeProps (Edge e1 n1 n2 l props) p = Edge e1 n1 n2 l $ updateProp p props

-- Given a graph PG, a node and a prop
-- Returns a new graph with the property prop of node changed if node is found
-- Returns the same graph if node is not found
defVprop :: PG -> Node -> Prop -> PG
defVprop (PG ns es) (Node n lab props) prop =
  do
    let node = searchNodeInGraph (PG ns es) n
    case node of
      Nothing -> PG ns es
      Just node -> PG (updateNode (updateNodeProps node prop) ns) es

-- Given a graph PG, a node and a label L
-- Returns a new graph with node's label set to L if node is found and has not a label
-- Returns an error message if node is not found or node has already a label set
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

-- Given a graph PG, an edge and a prop
-- Returns a new graph with the property prop of edge changed if edge is found
-- Returns the same graph if edge is not found
defEprop :: PG -> Edge -> Prop -> PG
defEprop (PG ns es) (Edge e n1 n2 lab props) prop =
  do
    let edge = searchEdgeInGraph (PG ns es) e
    case edge of
      Nothing -> PG ns es
      Just edge -> PG ns $ updateEdge (updateEdgeProps edge prop) es

-- Given a graph PG, a edge and a label L
-- Returns a new graph with edge's label set to L if edge is found and has not a label
-- Returns an error message if edge is not found or edge has already a label set
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

-- Given a graph PG, edge id e1, node id n1 and  node id n2
-- Returns a new PG with a new edge e1 from n1 to n2 appended to PG's edges
addEdge :: PG -> String -> String -> String -> PG
addEdge (PG nodes edges) e n1 n2 = PG nodes (Edge e n1 n2 "" [] : edges)

first :: (a, b) -> a
first (x, y) = x

second :: (a, b) -> b
second (x, y) = y

setLabelToEdge :: (Id, Label) -> [Edge] -> Edge
setLabelToEdge label (e : es)
  | edgeHasId (first label) e = do
    let newEdge = setEdgeLabel e (second label)
    case newEdge of
      Just newEdge -> newEdge
      Nothing -> e
  | otherwise = setLabelToEdge label es

setLabelsToEdges :: [(Id, Label)] -> [Edge] -> [Edge]
setLabelsToEdges [] es = es
setLabelsToEdges _ [] = []
setLabelsToEdges ls es = map (`setLabelToEdge` es) ls

setNodeProps :: PG -> [(Id, Prop)] -> PG
setNodeProps =
  foldl
    (\pg p -> defVprop pg (Node (first p) "" []) (second p))

setEdgeProps :: PG -> [(Id, Prop)] -> PG
setEdgeProps =
  foldl
    (\pg p -> defEprop pg (Edge (first p) "" "" "" []) (second p))

-- Given 4 paths for: rhoFile, lambdaFile, sigmaFile and propFile
-- Returns a graph PG created based on the files specifications
populate :: String -> String -> String -> String -> PG
populate rho lambda sigma props =
  let edges = parseRhoFile rho
      nodes = parseLambdaFileIntoNodes lambda edges
      labels = parseLambdaFileIntoLabels lambda edges
      nodeProps = parseSigmaFileIntoNodeProps sigma edges
      edgeProps = parseSigmaFileIntoEdgeProps sigma edges
      edgesWithLabels = setLabelsToEdges labels edges
      pg = setEdgeProps (setNodeProps (PG nodes edgesWithLabels) nodeProps) edgeProps
   in {-       pgWithNodeProps = setNodeProps pg nodeProps
            pgWithEdgeProps = setEdgeProps pg edgeProps -}
      pg

-- MAIN FUNCTION

main :: IO ()
main = do
  putStrLn "Input rhoFilePath"
  path <- getLine
  rhoFile <- readFile path

  putStrLn "Input lambdaFile"
  path <- getLine
  lambdaFile <- readFile path

  putStrLn "Input sigmaFile"
  path <- getLine
  sigmaFile <- readFile path

  {-  putStrLn "Input propFile"
    path <- getLine
    propFile <- readFile path -}

  let pg = populate rhoFile lambdaFile sigmaFile "propFile"
  print pg
  main