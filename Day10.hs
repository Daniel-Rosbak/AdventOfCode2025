import Data.List
{-}
part2 s = print (bests)
            where
                bests = p2best (head buttons) (head targets)--[p2best (buttons!!n) (targets!!n) | n <- [0..((length buttons)-1)]]
                buttons = map (map (map (str2int)))(map (map (wordson (==','))) (map (map init) (map (map tail) (map init (map tail ws)))))
                targets = map (joltage2state) (map init (map tail (map last ws)))
                ws = map words (lines s)

p2best xs t = p2works xs (replicate (length t) 0) t 0

p2works xs s t n = head (sort (map (\a -> if not (a == []) && safe (push a) t then if met (push a) t then (n+1) else p2works xs (push a) t (n+1) else 999) xs))
                    where
                        safe [] [] = True
                        safe (x:xs) (y:ys) = (x <= y)&& safe xs ys
                        met [] [] = True
                        met (x:xs) (y:ys) = (x == y)&& met xs ys
                        push x = foldr (\a b -> (take a b)++[(b!!a)+1]++(drop (a+1) b)) s x

joltage2state xs = map str2int (wordson (==',') xs)
-}
part1 s = print (sum bests)
            where
                bests = [best (subsequences (buttons!!n)) (targets!!n) | n <- [0..((length buttons)-1)]]
                buttons = map (map (map (str2int)))(map (map (wordson (==','))) (map (map init) (map (map tail) (map init (map tail ws)))))
                targets = map (lights2state 0) (map init (map tail (map head ws)))
                ws = map words (lines s)

best xs t = head(sort [length a | a <- xs, (works a [] t)])

works [] s t = (sort s) == (sort t)
works (x:xs) s t = works xs ((s++noton)\\alreadyon) t
                    where
                        noton = x \\ alreadyon
                        alreadyon = s `intersect` x

lights2state _ []  = []
lights2state n (x:xs) | x == '.' = lights2state (n+1) xs
                      | x == '#' = (n):(lights2state (n+1) xs)

str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs

--augmented words function
wordson :: (Char -> Bool) -> String -> [String]
wordson p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordson p s''
                            where (w, s'') = break p s'

-- input goes here:
testinput = ""
getinput = ""