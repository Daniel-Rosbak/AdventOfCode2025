
part2 s = print (p2solveproblems problems)
            where
                problems = p2makenums w
                w = (customwords (take 4 ls))++(map words (drop 4 ls))
                ls = map (++ " ") (lines s)

p2solveproblems (_,[]) = 0
p2solveproblems (a,b) = if op == "+"
                    then
                        (sum probs) + d
                    else
                        (foldr (*) 1 (filter (\y -> not(0 == y)) probs)) + d
                    where
                        op = head b
                        probs = map head a
                        d = p2solveproblems (map tail a, tail b)

p2makenums x = ([map (\s -> if (s == "") then 0 else str2int s) a | a <- take 4 x], head(drop 4 x))

customwords [[],[],[],[]] = [[],[],[],[]]
customwords [a,b,c,d] = [x:xd,y:yd,z:zd,w:wd]
                        where
                            [xd,yd,zd,wd] = customwords p
                            [x,y,z,w] = transpose t
                            p = [drop (s+1) a, drop (s+1) b, drop (s+1) c, drop (s+1) d]
                            t = (take s a, take s b, take s c, take s d)
                            s = (head [n | n <- [0..4], (a!!n) == ' ' && (b!!n) == ' ' && (c!!n) == ' ' && (d!!n) == ' '])

transpose (as,bs,cs,ds) = (replicate (4-(length p)) "")++p
                        where
                            p = foldr (++) [] (map words [[as!!n,bs!!n,cs!!n,ds!!n] | n <- [0..((length as)-1)]])


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
testinput = ""
getinput = ""