
isinvalid x = a == b
                where
                    (a,b) = splitAt ((length s)`div`2) s
                    s = show x

getinvalids [x,y] = [a | a <- [x..y], isinvalid a]

part1 s = print (sum [sum (getinvalids a) | a <- input])
        where
            input = range2numrange (splitranges s)


p2isrepeated x [] = True
p2isrepeated x xs = if x == (take l xs)
                    then p2isrepeated x (drop l xs)
                    else False
                    where
                        l = length x

p2isinvalid x = foldr (||) False [p2isrepeated a s | a <- xs]
                where
                    xs = [take a s| a <- [1..((length s)`div`2)]]
                    s = show x

p2getinvalids [x,y] = [a | a <- [x..y], p2isinvalid a]

part2 s = print (sum [sum (p2getinvalids a) | a <- input])
        where
            input = range2numrange (splitranges s)

str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs

range2numrange :: [String] -> [[Int]]
range2numrange [x,y] = [[str2int x, str2int y]]
range2numrange (x:y:xs) = [str2int x, str2int y]:a
                        where
                            a = range2numrange xs

splitranges [x] = [[x]]
splitranges (x:xs) = if x == ',' || x == '-'
                    then []:(a:b)
                    else (x:a):b
                    where
                        (a:b) = splitranges xs

-- input goes here:
testinput = ""
getinput = ""