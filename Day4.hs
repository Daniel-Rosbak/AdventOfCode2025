

forkliftable a (x,y) = rolls < 4
                        where
                            rolls = foldr (\a b -> if a == '@' then b+1 else b) 0 neighbours
                            neighbours = [(a!!(x + c))!!(y + d) | c <- [-1..1], d <- [-1..1], not ((c == 0) && (d == 0))]

part1 s = print (length [(a,b) | a <- [1..((length input)-2)], b <- [1..((length(head input))-2)],((input!!a)!!b) == '@', forkliftable input (a,b)])
            where
                input = [(foldr (\a b -> a:b) "" (replicate w '.'))]++(padded)++[(foldr (\a b -> a:b) "" (replicate w '.'))]
                padded = ["."++a++"." | a <- raw]
                w = (length (head raw)) + 2
                raw = lines s






str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs

-- input goes here:
testinput = ""
getinput = ""