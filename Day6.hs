
part2 s = print (ls)
            where
                --problems = p2makenums w
                --w = (customwords (take 4 ls))++(words(drop 4 ls))
                ls = lines s
{-
p2makenums x = ([map str2int a | a <- take 4 x], head (drop 4 x))

customwords [[],[],[],[]] = [[],[],[],[]]
customwords [a,b,c,d] = [x:xd,y:yd,x:xd,w:wd]
                        where
                            (xd,yd,zd,wd) = customwords p
                            [x,y,z,w] = (transpose t)
                            p = [drop (s+1) a, drop (s+1) b, drop (s+1) c, drop (s+1) d]
                            t = (take s a, take s b, take s c, take s d)
                            s = (head [n | n <- [0..4], (a!!n) == ' ' && (b!!n) == ' ' && (c!!n) == ' ' && (d!!n) == ' '])

transpose ([],[],[],[]) = []
transpose ((a:as),(b:bs),(c:cs),(d:ds)) = (a:b:c:d):transpose(as,bs,cs,ds)
-}
part1 s = print (solveproblems problems)
            where
                problems = makenums w
                w = map words ls
                ls = lines s

solveproblems (_,[]) = 0
solveproblems (a,b) = if op == "+"
                    then
                        (sum probs) + d
                    else
                        (foldr (*) 1 probs) + d
                    where
                        op = head b
                        probs = map head a
                        d = solveproblems (map tail a, tail b)

makenums x = ([map str2int a | a <- take 4 x], head (drop 4 x))

str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs


-- input goes here:
