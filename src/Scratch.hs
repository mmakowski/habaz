{-
data State1 = State1
data State2 = State2
data State3 = State3

data Msg = Msg1 | Msg2 | Msg3

trans :: Msg -> a -> b
trans Msg1 = s12
trans Msg2 = s23

s12 State1 = State2
s23 State2 = State3
-}