
newtype MyInt = MI Int deriving (Eq, Show)

f :: MyInt -> MyInt
f (MI n) = MI (n + 1)
