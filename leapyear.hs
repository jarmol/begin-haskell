module Leapyear (main)
where

leapyear :: Integral a => a -> String
leapyear year
  | mod year 400 == 0 = " is a leap year."
  | mod year 100 == 0 = " is not a leap year."
  | mod year 4 == 0 = " is a leap year."
  | otherwise = " is not a leap year."

makeRow :: Int -> String
makeRow year =
   "The year " ++ show year ++ leapyear year

ylist = [1900,2000,2012] ++ [2022..2026]


rlist :: [String]
rlist  = map makeRow ylist 
slist :: [[Char]]
slist = map (++ "\n") rlist

main :: IO ()
main =
   putStrLn $ concat slist
