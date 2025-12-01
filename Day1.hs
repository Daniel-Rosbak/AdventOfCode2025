within990 x = if x > 99
                then within990 (x - 100)
                else if x < 0
                    then within990 (x + 100)
                    else x

str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs

inst2num (x:xs) = if x == 'L'
                    then (0 - str2int xs)
                    else (str2int xs)

addrot x y = within990 (x + y)

insts2rots :: [Int] -> [Int]
insts2rots [x] = [within990 x]
insts2rots (x:xs) = (within990 x):(map (addrot x) (insts2rots xs))

part1 = print (length [x | x <- (insts2rots input), x == 0])
        where
            input = 50:(map (inst2num) getinput)






iszero (x,y) = if x == 0
                then (x,y+1)
                else if x == (-1)
                    then (99,y)
                    else if x == 100
                        then (0,y+1)
                        else (x,y)

r (x,y) = iszero (x+1,y)
l (x,y) = iszero (x-1,y)

spin (x,y) 0 = (x,y)
spin (x,y) z = if z < 0
                then l (a,b)
                else r (c,d)
                where
                    (a,b) = spin (x,y) (z+1)
                    (c,d) = spin (x,y) (z-1)

runinsts [x] = spin (50,0) x
runinsts (x:xs) = spin (a,b) x
                where
                    (a,b) = runinsts xs

part2 = print (skips)
        where
            skips = runinsts (reverse input)
            input = (map (inst2num) getinput)

-- input goes here:
getinput = lines ""