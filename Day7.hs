import Data.List

part2 s = print (timelines)
            where
                timelines = sum (quantumsplit rows)
                rows = reverse [ls!!n | n <- [0..((length ls)-1)], (n `mod` 2) == 0]
                ls = (lines s)

quantumsplit [x] = (replicate (length (take n x)) 0)++[1]++(replicate (length(drop (n+1) x)) 0)
                where
                    n = head [a | a <- [0..((length x) -1)], (x!!a)=='S']
quantumsplit (x:xs) = newtimelines
                where
                    newtimelines = foldr (timelinesplit) timelines tosplit
                    tosplit = [n | n <- [0..((length x)-1)], (x!!n) == '^']
                    timelines = quantumsplit xs

timelinesplit y x = [if (n-1) == y || (n+1) == y then (x!!n)+(x!!y) else if n == y then 0 else (x!!n) | n <- [0..((length x)-1)]]

part1 s = print (splits)
            where
                out = (foldr (\a b -> a++"\n"++b) "" (reverse beams))
                (beams, splits) = split rows
                rows = reverse [ls!!n | n <- [0..((length ls)-1)], (n `mod` 2) == 0]
                ls = (lines s)

split [x] = ([(take n x)++"|"++(drop (n+1) x)],0)
            where
                n = head [a | a <- [0..((length x) -1)], (x!!a)=='S']
split (x:xs) = (xbeamed:rest, splits+s)
            where
                xbeamed = foldr (placebeam) x allbeamsnodupes
                allbeamsnodupes = map head(group(sort(allbeams)))
                splits = (length allbeams) - (length beams)
                allbeams = foldr (++) [] [if ((x!!n) == '^') then [n-1,n+1] else [n] | n <- beams]
                beams = [n | n <- [0..((length above)-1)], (above!!n) == '|']
                above = head rest
                (rest,s) = split xs

placebeam y x = (take y x)++"|"++(drop (y+1) x)

-- input goes here:
testinput = ""
getinput = ""