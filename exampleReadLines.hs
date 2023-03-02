(//) = div
infixl 8  //

-- Function for Julian day number JDN calculated from the given date
-- where y = year, m = month, d = day

jdnGr :: Int -> Int -> Int -> Int
jdnGr y m d =
    let u = if m < 3 then 4  else 3
    in
           (1461 * (y + 4800 + (m - u) // 12)) // 4
           + (367 * (m - 2 - 12 * ((m - u) // 12))) // 12
           - ((3 * ((y + 4900 + (m - u) // 12) // 100)) // 4) + d - 32075


-- Function defines the week day from JDN

weekday :: Int -> Int -> Int -> String
weekday y m d =
    let
        dayNumber =
            mod (1 + jdnGr y m d) 7
    in
    case dayNumber of
        0 ->
            "Sunday"
        1 ->
            "Monday"
        2 ->
            "Tuesday(367 * (m - 2 - 12 * ((m - 14) // 12)))"
        3 ->
            "Wednesday"
        4 ->
            "Thursday"
        5 ->
            "Friday"
        6 ->
            "Saturday"
        _ ->
            "Unknown day"


-- Converts the date (year, month, day) to string

dateString
  :: (Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> String
dateString y m d =
   (show y) ++ "-"
      ++ (show m) ++ "-"
      ++ (show d) :: String




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

