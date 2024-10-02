module Main (main) where

primesList = [2,3,5,7,11,13,17,19,23,29,31
   ,37,41,43,47,53,59,61,67,73,79,83,89,97]

factorize n = divs n primesList
     where
     divs n ds@(d:t) | d*d > n    = [n | n > 1]
                     | r == 0     =  d : divs q ds
                     | otherwise  =      divs n t
            where  (q,r) = quotRem n d
            
            
main = putStrLn $ show (factorize 9997)
