data Exp  =  Num Int | Plus Exp Exp | Minus Exp Exp | Times Exp Exp deriving Read

evaluate :: Exp -> Int
evaluate (Num x)     = (x)
evaluate (Plus x y)  = (evaluate x) + (evaluate y)
evaluate (Minus x y) = (evaluate x) - (evaluate y)
evaluate (Times x y) = (evaluate x) * (evaluate y)