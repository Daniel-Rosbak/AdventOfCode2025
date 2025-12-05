import Data.List
import Data.Maybe

part2 s = print (freshids freshranges)
            where
                freshranges = ranges2nums a
                (a,_) = splitinput input
                input = lines s

freshids [] = 0
freshids (x1:x2:xs) = (x2-x1+1) + (freshids xs)

part1 s = print (onlyfresh freshranges ingredients)
            where
                ingredients = listofstr2int b
                freshranges = ranges2nums a
                (a,b) = splitinput input
                input = lines s

onlyfresh a b = length [1 | ing <- b, inrange ing a] 

inrange ing [] = False
inrange ing (x1:x2:xs) = (ing >= x1 && ing <= x2) || (inrange ing xs)

ranges2nums [x] = [b,c]
                    where
                        (b,c) = (str2int(take hyphen x), str2int(drop (hyphen+1) x))
                        hyphen = fromJust (elemIndex '-' x)
ranges2nums (x:xs) = merge d (b,c)
                    where
                        (b,c) = (str2int(take hyphen x), str2int(drop (hyphen+1) x))
                        hyphen = fromJust (elemIndex '-' x)
                        d = ranges2nums xs

merge x (y1,y2) = if t
                    then
                        d
                    else
                        y1:y2:d
                    where
                        (d,t) = mergeranges x (y1,y2)

mergeranges [] (y1,y2) = ([],False)
mergeranges (x1:x2:xs) (y1,y2) = if x1 >= y1 && y2 <= x2 && x1 <= y2
                                    then
                                        (merge d (y1,x2), True)
                                    else if y1 >= x1 && x2 <= y2 && y1 <= x2
                                        then
                                            (merge d (x1,y2), True)
                                        else if y1 <= x1 && y2 >= x2
                                            then
                                                (merge d (y1,y2), True)
                                            else if x1 <= y1 && x2 >= y2
                                                then
                                                    (merge d (x1,x2), True)
                                                else
                                                    (x1:x2:d, t || False)
                                where
                                    (d,t) = mergeranges xs (y1,y2)

splitinput x = (a,b)
                where
                    (a,b) = (take c x, drop (c+1) x)
                    c = fromJust (elemIndex "" x)

listofstr2int [] = []
listofstr2int (x:xs) = (str2int x):(listofstr2int xs)

str2int [x] = (fromEnum x) - 48
str2int (x:xs) = ((fromEnum x) - 48) * (10 ^ (length xs)) + str2int xs


-- input goes here:
testinput = ""
getinput = ""