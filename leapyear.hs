module Leapyear (main)
where

leapyear :: Integral a => a -> String
leapyear year
  | mod year 400 == 0 = " is a leap year."
  | mod year 100 == 0 = " is not a leap year."
  | mod year 4 == 0 = " is a leap year."
  | otherwise = " is not a leap year."

leaprule = [
   "______________________________________________________________"
   ,""
   ,"Every year that is exactly divisible by four is a leap year,"
   ,"except for years that are exactly divisible by 100, but these"
   ,"centurial years are leap years if they are exactly divisible"
   ,"by 400. For example, the years 1700, 1800, and 1900 are not"
   ,"leap years, but the years 1600 and 2000 are."
   ,""
   ,"See https://en.wikipedia.org/wiki/Leap_year#Gregorian_calendar" 
  ]

makeRow :: Int -> String
makeRow year =
   "The year " ++ show year ++ leapyear year

ylist :: [Int]
ylist = [1600,1700..2000] ++ [2020..2030]

tlist :: String
tlist = unlines $ map makeRow ylist

main :: IO ()
main = do
   putStrLn tlist
   putStrLn $ unlines leaprule
