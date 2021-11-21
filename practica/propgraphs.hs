-- TYPE SYNONYMS

type Label = String

type Id = String

-- CUSTOM DATA TYPES

data Val = String | Int | Double | Bool | Date

data Prop = Name Label | Value (Maybe Val)

data Node
  = N Id
  | Type Label
  | Props [Prop]

data Edge
  = V Id
  | Origin Node
  | EdgeProps [Prop]

data PG
  = Nodes [Node]
  | Edges [Edge]

-- POPERTY GRAPH (PG) INSTANCES

instance Show PG

-- I/O FUNCTIONS

-- PROPERTY GRAPHS METHODS

populate :: String -> String -> String -> String -> PG
addEdge :: PG -> Edge -> Prop -> PG
defVprop :: PG -> Node -> Prop -> PG
defEProp :: PG -> Edge -> Prop -> PG
defVlabel :: PG -> Node -> Prop -> PG
defElabel :: PG -> Edge -> Prop -> PG
showGraph :: PG

-- PROPERTY GRAPHS QUERIES