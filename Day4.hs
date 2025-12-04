

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




removable x = (pad [[c | (c,d) <- tuples] | tuples <- tupled],sum[sum[d | (c,d) <- tuples, d == 1] | tuples <- tupled])
                where
                    tupled = tupledmap x


tupledmap x = [[if ((x!!a)!!b) == '@' then if forkliftable x (a,b) then ('.',1) else ('@',0) else ('.',0) | b <- [1..((length(head x))-2)]] | a <- [1..((length x)-2)]]

pad x = [(foldr (\a b -> a:b) "" (replicate w '.'))]++(padded)++[(foldr (\a b -> a:b) "" (replicate w '.'))]
        where
            padded = ["."++a++"." | a <- x]
            w = (length (head x)) + 2

remove x = if b == 0
            then
                b
            else
                b+d
                where
                    (a,b) = removable x
                    d = remove a

part2 s = print (remove input)
            where
                input = pad raw
                raw = lines s

-- input goes here:
testinput = ""
getinput = ""