import Data.List

part1 s = print (top3)
            where
                top3 = take 3 (reverse (sort (map (length) (group (sort (map (head) (map (drop 3) matched)))))))
                matched = match (map (++[0]) coords) 0
                coords = map str2coords ls
                ls = lines s

match [x1,x2,x3,x4] _ = [x1,x2,x3,x4]
match ([x1,x2,x3,x4]:xs) n = if x4 == 0
                                then
                                    if tag == 0
                                        then
                                            ([x1,x2,x3,(n+1)]):(match ((take ind xs)++([[y1,y2,y3]++[n+1]])++(drop (ind+1) xs)) (n+1))
                                        else
                                            ([x1,x2,x3,tag]):(match xs n)
                                else
                                    [x1,x2,x3,x4]:(match xs n)
                                where
                                    [y1,y2,y3,y4] = xs!!ind
                                    (_,tag,ind) = head (sort distances)
                                    distances = [(\[z1,z2,z3,z4] -> (sqrt (fromIntegral (((z1-x1)^2)+((z2-x2)^2)+((z3-x3)^2))),z4,n)) (xs!!n) | n <- [0..((length xs)-1)]]

str2coords x = map str2int [take (commas!!0) x, drop ((commas!!0) +1) (take (commas!!1) x), drop ((commas!!1)+1) x]
            where
                commas = elemIndices ',' x

str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs

-- input goes here:
testinput = ""
getinput = ""