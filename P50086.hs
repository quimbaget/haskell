data Queue a = Queue [a] [a]
  deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue as bs) = Queue as (a : bs)

pop :: Queue a -> Queue a
pop (Queue [] bs) = Queue (reverse $ tail bs) []
pop (Queue as bs) = Queue (tail as) bs

top :: Queue a -> a
top (Queue [] bs) = top $ Queue (reverse bs) []
top (Queue (a : as) _) = a

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Functor Queue where
  fmap f (Queue q1 q2) = Queue (fmap f q1) (fmap f q2)

translation :: Num b => b -> Queue b -> Queue b
translation num = fmap (+ num)

instance Monad Queue

Queue [a] [b] >>= f = Queue (xs >>= f) (ys >>= f)
