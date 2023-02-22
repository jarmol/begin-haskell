import Solarlib (jdnGr, weekday)


 
main :: IO ()
main = do
         print    "Year: "
         year :: Int <- readLn
         print  "Month: "
         month :: Int <- readLn
         print $ "Day: "
         day :: Int <- readLn
         let 
             dateStr = (show year) ++ "-" ++ (show month) ++ "-" ++ (show day) :: String
         putStr $ dateStr ++ " " ++  show (weekday year month day)
         putStrLn $ "  Julian date number (JDN) = " ++ show (jdnGr year month day)
