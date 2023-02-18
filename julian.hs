import Solarlib (jdnGr, weekday)



year = 2023
month = 2
day = 17


main :: IO ()
main = do
         putStrLn $ "Julian date number JDN = " ++ show (jdnGr year month day)
         putStrLn $ " on 2023-02-17  " ++ show (weekday year month day)
