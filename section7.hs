--33--
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)
nadd :: Nat -> Nat -> Nat
nadd x Zero = x
nadd x (Succ y) = Succ $ nadd x y

--a--
nfib :: Nat -> Nat
nfib Zero = Zero
nfib (Succ Zero) = Succ Zero
nfib (Succ (Succ x)) = nadd (nfib (Succ x)) (nfib x)
--b--
foldn :: (a -> a) -> a -> Nat -> a
foldn f x Zero = x
foldn f x (Succ n) = f (foldn f x n)

nfib' :: Nat -> Nat
nfib' Zero = Zero
nfib' (Succ Zero) = Succ Zero
nfib' (Succ x) = 
