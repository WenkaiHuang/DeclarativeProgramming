module Assignment1 (subst, interleave, unroll) where
subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b (x:xs) = if a == x
          then b:(subst a b xs)
          else x:(subst a b xs)


interleave :: [t] -> [t] -> [t]
interleave [] [] = []
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x:y:interleave xs ys


unroll :: Int -> [a] -> [a]
unroll n (x:xs) = if n<=0
                then [] 
                else x:unroll (n-1)(xs++[x])
