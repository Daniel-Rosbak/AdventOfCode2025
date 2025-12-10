import Data.List

part1 s = print (maxrect)
            where
                maxrect = head (head (reverse (sort (map (reverse) (map (sort) (map (rects redtiles) redtiles))))))
                redtiles = map str2coords ls
                ls = lines s

--rects :: [[Int]] -> [Int] -> [Int]
rects [] _ = []
rects ([y1,y2]:ys) [x1,x2] = area:(rects ys [x1,x2])
                where
                    area = (((abs(x1-y1))+1)*((abs(x2-y2))+1))

str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs

str2coords x = map str2int [take (commas!!0) x, drop ((commas!!0)+1) x]
            where
                commas = elemIndices ',' x

-- input goes here:
testinput = ""
getinput = ""