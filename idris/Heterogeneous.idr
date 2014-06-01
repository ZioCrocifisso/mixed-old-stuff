data Het : List Type -> (Type -> Type) -> Type where
  Nil : Het [] f
  Cons : f a -> Het ts f -> Het (a :: ts) f

data Test : Type -> Type where
  Testv : a -> Test a

Het0 : List Type -> Type
Het0 l = Het l id

testList' : Het0 [ Int, String ]
testList' = Cons 0 $ Cons "abc" Nil

testList : Het [ Int, String ] Test
testList = Cons (Testv 0) $ Cons (Testv "abc") Nil
