# List operations

## Finite lists

ghci> head [1 .. 6]

1

ghci> tail [1 .. 6]

[2,3,4,5,6]

ghci> last [1 .. 6]

6

ghci> init [1 .. 6]

[1,2,3,4,5]

ghci> reverse [1 .. 8]

[8,7,6,5,4,3,2,1]

ghci> [1..6] !! 3

4

ghci> ['a' .. 'v']

"abcdefghijklmnopqrstuv"

ghci> ['a' .. 'v'] !! 3

'd'

ghci> 7 : [1..6]

[7,1,2,3,4,5,6]

ghci> foldl (+) 0 [1,4,6,7,11]

29

ghci> foldl (*) 1 [1 .. 6]

720

ghci> filter (> 3) [1 .. 8]

[4,5,6,7,8]

ghci> filter (\n -> mod n 2 == 1) [1 .. 9]

[1,3,5,7,9]

ghci> filter (\n -> mod n 2 == 0) [1 .. 9]

[2,4,6,8]

ghci> map (\x -> sqrt x) [2 .. 6]

[1.4142135623730951,1.7320508075688772,2.0,2.23606797749979,2.449489742783178,]

