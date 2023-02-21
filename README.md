# Good references
<a href="">Zwon-Haskell Reference</a>

# Integer division in Haskell vs Elm

I translated a little piece of code in elm language to haskell.
There rose a problem as the code contained division with negative integers.
Elm has the operator '//' but in haskell we have instruction div.
I learned recently how to redefine div to operator '//' and wanted to use
it in haskel expecting that the same code could be used in both languages
without need for  much changes. However, the same equation as such did'nt
work always correctly. It has not caused runtime errors, just wrong 
calculation results for certain dates.


Here is explained how I have fixed the code.   


-- Declaring the operator '//' to be used instead of div
<code>
(//) = div
infixl 8  //
</code>

* Function jdnGr for Julian day number JDN calculated from the given date
where y = year, m = month, d = day
* The original formula of Elm version would give wrong results if used in
Haskell. The reason is the difference in integer division of the
negative numbers. For that reason the correction factor 'u' was added
to the Haskell formula in function jdnGr 

------------------------------------------

## Integer division using operator '//' in Elm

------------------------------------------

<code>
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
</code>
---------------------------------------------------

## Integer division in Haskell 

Same formula used as in Elm gives incorrect results
with operator '//' declared to be same as 'div'


---------------------------------------------------

### These are incorrect results

<code>
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
</code>


## Correction of the formula

* January m =1,   (m - u) // 12 == -1, u = 4
* February m = 2, (m - u) // 12 == -1, u = 4
* March ... December m = 3 ... 12, 
  (m - u) // 12 == 0,  u = 3
