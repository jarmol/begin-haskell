module Main where

import Solarlib (jdnGr
   , dateString
   , julianDay
   , julianCentury
   , nonIntRem
   , geomMeanLong
   , sunTrueLong
   , sunTrueAnom
   , sunEqOfCtr
   , sunAppLong
   , meanObliqEcliptic
   , obliqCorr, sunDeclin, eqTime, haSunrise, solarNoonLST, showtime, sunriseLST, sunsetLST )

import Numeric ( showFFloat )
import Text.Printf ( printf )
import Data.Maybe (fromJust)
import Data.Char (GeneralCategory(DecimalNumber))


-- Julian day vs local time at timezone +2
-- Date 2024-07-03

-- Local time Julian day
-- 02:00       2460494,50
-- 14:00       2460495,00
-- 02:00       2450495,50 next day UTC time 00:00


putsRow :: Int -> Int -> Int -> Double -> Double -> IO ()
putsRow y m d tz tloc =
  do
    let jD = julianDay y m d tz tloc
        jC = julianCentury y m d tz tloc
        gL = sunTrueLong y m d tz tloc
    --  gA = sunTrueAnom y m d tz tloc
        sunEC  = sunEqOfCtr y m d tz tloc
        eqT = eqTime y m d tz tloc
        srHA = haSunrise y m d tz tloc
        noonT = solarNoonLST y m d tz tloc
        sunriseT = sunriseLST y m d tz tloc
        sunDec = sunDeclin y m d tz tloc
        sunsetT = sunsetLST y m d tz tloc
    putStr (dateString y m d)
    printf "%6.2f" (tloc - tz)
    printf "%7.2f " tloc
    printf "%11.3f  " jD
    printf "%8.6f " jC
 -- printf "%10.4f" gL
    printf "%10.5f" sunDec
    printf "%11.7f" eqT
    printf "%9.3f" srHA
    printf "%10s" (showtime sunriseT)
    printf "%10s" (showtime noonT)
    printf "%10s\n" (showtime sunsetT)



main :: IO ()
main = do
    putStr "\nDate      UTC   Local  Julian      Julian.    "
    putStrLn "Sun       Time-               Sunrise   Noontime  Sunset"
    putStr "          time  time   day         century    "
    putStrLn "declinat  equation    HA °    time      time      time"
    putsRow 2024 7 2 2.0 24.0
    putsRow 2024 7 3 2.0 2.0
    putsRow 2024 7 3 2.0 8.0
    putsRow 2024 7 3 2.0 12.0
    putsRow 2024 7 3 2.0 14.0
    putsRow 2024 7 3 2.0 20.0

