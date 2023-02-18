I translated a little piece of code in elm language to haskell.
There rose a problem as the code contained division with negative integers.
Elm has the operator '//' but in haskell we have instruction div.
I learned recently how to redefine div to operator '//' and wanted to use
it in haskel expecting that the same code could be used in both languages
without need for  much changes. However, the same equation as such did'nt
work always correctly. It has not caused runtime errors, just wrong 
calculation results for certain dates.

Here is explained how I have fixed the code.   

Calculation of Julia daynumber (JDN) for the given date
A. Version in Elm language
---------------------------------
module NewJulia exposing (jdnGr)

jdnGr : Int -> Int -> Int -> Int
jdnGr y m d =
    (1461 * (y + 4800 + (m - 14) // 12))
        // 4
        + (367 * (m - 2 - 12 * ((m - 14) // 12)))
        // 12
        - ((3 * ((y + 4900 + (m - 14) // 12) // 100)) // 4)
        + d
        - 32075

--------------------------------

B. Version in Haskell language
--------------------------------

module Solarlib where 


-- Declaring the operator '//' to be used instead of div

(//) = div
infixl 8  //

-- Function for Julian day number JDN calculated from the given date
-- where y = year, m = month, d = day
-- The formula of Elm version would give wrong results if used in
-- Haskell. The reason is the difference in integer division of the
-- negative numbers. For that reason the correction factor u was added
-- to the Haskell formula in function jdnGr 

jdnGr :: Int -> Int -> Int -> Int
jdnGr y m d =
    let u = if m < 3 then 4  else 3
    in
           (1461 * (y + 4800 + (m - u) // 12)) // 4
           + (367 * (m - 2 - 12 * ((m - u) // 12))) // 12
           - ((3 * ((y + 4900 + (m - u) // 12) // 100)) // 4) + d - 32075

---------------------------
Integer division using operator '//' in Elm
------------------------------------------
> import NewJulia exposing (jdnGr)
> y = 2023
> m = 1
> d = 31
> (m - 14) // 12
-1 : Int
> m=2
> (m - 14) // 12
-1 : Int
> m = 3
> (m - 14) // 12
0 : Int
> m= 12
> (m - 14) // 12
0 : Int
> 
------------------
Integer division in Haskell 
Same formula as in Elm user gives incorrect results
with operator '//' declared to be same as 'div'
---------------------------------------------------
GHCi, version 9.2.5: https://www.haskell.org/ghc/  :? for help
ghci> :l Solarlib.hs
[1 of 1] Compiling Solarlib         ( Solarlib.hs, interpreted )
Ok, one module loaded.
ghci> y=2023
ghci> m = 1
ghci> (m - 14) // 12
-2
ghci> m = 2
ghci> (m - 14) // 12
-1
ghci> m = 3
ghci> (m - 14) // 12
-1
ghci> m = 12
ghci> (m - 14) // 12
-1
ghci> 
----------------------
