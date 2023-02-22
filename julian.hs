import Solarlib (jdnGr, weekday, dateString)


 
main :: IO ()
main = do
         print    "Year: "
         year :: Int <- readLn
         print  "Month: "
         month :: Int <- readLn
         print $ "Day: "
         day :: Int <- readLn
         putStr $ dateString year month day ++ ", " ++  show (weekday year month day)
         putStrLn $ "  Julian date number (JDN) = " ++ show (jdnGr year month day)
