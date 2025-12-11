import Data.List

part2 s = print (p2dacfft ws "svr")
            where
                ws = map (map (take 3)) (map words (lines s))

p2dacfft x s = p2path x False False s

p2path x d f s = outs + (sum (map (p2path nextx dy fy) nexts))
            where
                outs = length [1 | w <- devices, (w == "out") && dy && fy]
                nexts = [z | z <- devices, (z`elem`(map head nextx)) && not (z == "out")]
                fy = f || (s == "fft")
                dy = d || (s == "dac")
                nextx = filter (\a -> not((head a) == s)) x
                devices = foldr (++) [] [ys | (y:ys) <- x, y == s]

part1 s = print (path ws "you")
            where
                ws = map (map (take 3)) (map words (lines s))

path x s = outs + (sum (map (path nextx) nexts))
            where
                outs = length [1 | w <- devices, w == "out"]
                nexts = [z | z <- devices, (z`elem`(map head nextx)) && not (z == "out")]
                nextx = filter (\a -> not((head a) == s)) x
                devices = foldr (++) [] [ys | (y:ys) <- x, y == s]

-- input goes here:
testinput = ""
test2input = ""
getinput = ""