data List t = Nil : Cona t (List t)
    deriving Show

instance Eq t => Eq (List t) where
    (==) Nil Nil = True
    (==) (Cons x xs) (Cons y ys) = (x == y) && (xs == ys)
    (==) _   _   = False

data Shape = Circle Float | Rectangle Float Float | Square Float
    deriving (Show)

instance Eq Shape where
    (==) (Circle x) (Circle y) = x == y
    (==) (Rectangle x y) (Rectangle w z) = (x == w && y == z) || (x == z & y == w)
    (==) (Square x) (Square y) = x == y
    (==) (Square x) (Rectangle y z) = x == y && x == z
    --(==) (Rectangle y z) ()
    (==) _          _ = False

isRound :: Shape -> Bool
isRound (Circle r) = True
isRound (Rectangle x y) = False
isRound (Square x) = False