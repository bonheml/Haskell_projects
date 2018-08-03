import Data.List

type Transition = ((Integer, String, String),(Integer, String))
type PDA = (Integer, [Integer], [Transition])
type Configuration = (Integer, String, String)
data Result = Accept | Reject deriving Show


applyTransition :: Configuration -> Transition -> Configuration
applyTransition (_, bs, cs) ((_, ws, xs), (y, zs)) =
    let stack = zs ++ (drop (length xs) cs)
        input = drop (length ws) bs
    in (y, input, stack)


isValidTransition :: Configuration -> Transition -> Bool
isValidTransition (a, bs, cs) ((x, ys, zs), _) =
    x == a && isPrefixOf ys bs && isPrefixOf zs cs


getTransitions :: Configuration -> [Transition] -> [Transition]
getTransitions c ts = filter (\t -> isValidTransition c t) ts


getConfigurations :: Configuration -> PDA -> [Configuration]
getConfigurations c (_, _, xs) = [applyTransition c t | t <- getTransitions c xs]


evaluate :: Configuration -> PDA -> Bool
evaluate (a, b, c) (x, ys, zs)
    | elem a ys && b == [] && c == [] = True
    | otherwise                       = evaluateAll cfgs (x, ys, zs)
    where cfgs = getConfigurations (a, b, c) (x, ys, zs)


evaluateAll :: [Configuration] -> PDA -> Bool
evaluateAll cfgs pda = foldr (||) False (map (\cfg -> evaluate cfg pda) cfgs)


run :: PDA -> String -> Result
run (x, ys, zs) input
   | res == True = Accept
   | otherwise   = Reject
   where res     = evaluate (x, input, []) (x, ys, zs)