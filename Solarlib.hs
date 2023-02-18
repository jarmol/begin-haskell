module Solarlib where 


-- Declaring the operator '//' to be used instead of div

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



-- Function to define whether the given year is leap year

isLeapYear :: Integral a => a -> Bool
isLeapYear y =
    if mod y 400 == 0 then True
    else if mod y 100 == 0 then False
    else if mod y 4 == 0 then True 
    else False


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


-- testing

-- main = jdnGr 2023 2 13  -- 2459989

