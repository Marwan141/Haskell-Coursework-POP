data Natural = Zero | Succ Natural deriving (Show, Eq, Ord)

add1 :: Natural -> Natural -> Natural
add1 Zero n = n
add1 (Succ m) n = Succ (add1 m n)

minus1 :: Natural -> Natural -> Natural
minus1 Zero n = Zero
minus1 m Zero = m
minus1 (Succ m) (Succ n) = minus1 m n
