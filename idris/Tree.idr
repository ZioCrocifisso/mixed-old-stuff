module Tree

data Tree a = Leaf | Node (Tree a) a (Tree a)

data ElemTree : a -> Tree a -> Type where
  ElemHere  : {x : a} -> {l, r : Tree a} ->
              ElemTree x (Node l x r)
  ElemLeft  : {x, y: a} -> {l, r : Tree a} ->
              ElemTree x l -> ElemTree x (Node l y r)
  ElemRight : {x, y: a} -> {l, r : Tree a} ->
              ElemTree x r -> ElemTree x (Node l y r)

testTree : Tree Int
testTree = Node (Node Leaf 0 Leaf)
                1
                (Node Leaf 2 (Node Leaf 3 Leaf))

tt0 : ElemTree 0 testTree
tt0 = ElemLeft ElemHere

tt1 : ElemTree 1 testTree
tt1 = ElemHere

tt2 : ElemTree 2 testTree
tt2 = ElemRight ElemHere

tt3 : ElemTree 3 testTree
tt3 = ElemRight $ ElemRight ElemHere

elemInTree : (DecEq a) => (x : a) -> (t : Tree a) -> Maybe (ElemTree x t)
elemInTree _ Leaf = Nothing
elemInTree x (Node l y r) with (decEq x y)
  | (Yes prf) = Just $ rewrite prf in ElemHere { x = y }
  | (No _) = case elemInTree x l of
                Just et => Just $ ElemLeft et
                Nothing => case elemInTree x r of
                                Just et => Just $ ElemRight et
                                Nothing => Nothing
