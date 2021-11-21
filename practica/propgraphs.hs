type Label = String

type Id = String

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