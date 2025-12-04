import Data.List
import Data.Maybe

maxjoltage [x,y] = str2int [x,y]
maxjoltage (x:xs) = maximum [maximum [str2int [x,b] | b <- xs], maxjoltage xs]

part1 s = print (sum (map (maxjoltage) input))
            where
                input = lines s


findmax :: String -> Int -> Int -> String
findmax x y z   | y == 0 = []
                | (length x) == y = x
                | (length x) > y  = if isNothing a
                                    then
                                        findmax x y (z-1)
                                    else if ((length x)-(fromJust a)) > (y-1)
                                        then 
                                            (x!!(fromJust a)):findmax (drop ((fromJust a) + 1) x) (y-1) 9
                                        else
                                            findmax x y (z-1)
                                    where
                                        a = findIndex (==(head (show z))) x

p2maxjoltage :: String -> Int
p2maxjoltage x = str2int (findmax x 12 9)

part2 s = print (sum (map (p2maxjoltage) input))
            where
                input = lines s



str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs

-- input goes here:
testinput = ""
getinput = ""