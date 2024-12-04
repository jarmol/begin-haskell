module SolarCurrent
   (julianCentury
   , geomMeanLong
   , geomMeanAnom
   , eccOrbit
   , sunEqOfCtr
   , sunTrueLong
   , sunAppLong
   , meanObliqEcliptic
   , obliqCorr
   , sunDeclin
   , yVar
   , eqTime
   , haSunrise
   , solarNoonLST
   , sunriseLST
   , sunsetLST
   , trueSolarTime
   , hourAngle
   , solarZenithAngle
   , atmosRefract
   , showtime
   , solAzimuth
   , sunlightDuration
   , nonIntRem
   , showLocalTime
   )
   where

import Data.Time ( toGregorian, UTCTime(utctDay), hoursToTimeZone )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds, POSIXTime)
--import Text.XHtml (center)

julianCentury :: UTCTime -> Double
julianCentury tcurrent =
    let (year, month, day) = toGregorian . utctDay $ tcurrent
        posixSeconds = utcTimeToPOSIXSeconds tcurrent
        sinceEpochBegin = posixSeconds/3600/24
        numberJD = 2440587.5 + sinceEpochBegin
        juliCent = (numberJD - 2451545) / 36525
        sJC = show juliCent
    --in   fromUTC juliCent
        len = length sJC
    in  read $ take (len - 1) sJC :: Double

-- Geom. average sun longitude
geomMeanLong, geomMeanAnom :: RealFrac a => a -> a
geomMeanLong cent =
     nonIntRem (280.46646 + (cent * (36000.76983 + cent * 0.0003032))) 360

geomMeanAnom cent =
     nonIntRem (357.52911 + cent * (35999.05029 - 0.0001537 * cent)) 360



eccOrbit :: Fractional a => a -> a
eccOrbit cent =
     0.016708634 - cent * (0.000042037 + 0.0000001267 * cent)

sunEqOfCtr :: (Floating a, RealFrac a) => a -> a
sunEqOfCtr cent =
  let gA = geomMeanAnom cent
  in  sin (  rad gA ) * ( 1.914602 - cent * ( 0.004817 + 0.000014 * cent ))
      + sin ( rad  2 * gA ) * ( 0.019993 - 0.000101 * cent )
      + sin ( rad  3 * gA ) * 0.000289


deg r = 180.0 * r / pi

sunTrueLong, sunAppLong :: (RealFrac a, Floating a) => a -> a
sunTrueLong cent =
    geomMeanLong  cent + sunEqOfCtr cent

sunAppLong cent =
    sunTrueLong cent - 0.00569 -
    0.00478 * sin ( rad (125.04 - 1934.136 * cent))


rad, deg :: Floating a => a -> a
rad g = pi*g/180.0

-- | deg

meanObliqEcliptic :: Fractional a => a -> a
meanObliqEcliptic cent =
     23 +
        (26 +
         (21.448 - cent * (46.815 + cent * (0.00059 - cent * 0.001813))) / 60) /
        60


obliqCorr :: Floating a => a -> a
obliqCorr  cent =
    meanObliqEcliptic cent
     + 0.00256 * cos (rad (125.04 - 1934.136 * cent))

-- | deg
sunDeclin :: (Floating c, RealFrac c) => c -> c
sunDeclin cent =
    deg . asin $
    sin (rad $ obliqCorr  cent) * sin (rad $ sunAppLong cent)


-- Y-variable
yVar :: Floating a => a -> a
yVar cent = tan (rad (obliqCorr cent) / 2) * tan (rad (obliqCorr cent)/2)


-- Equation of time
eqTime :: (Floating a, RealFrac a) => a -> a
eqTime  cent =
     4 * deg (yVar cent * sin (2 * rad (geomMeanLong cent))
   - 2 * eccOrbit cent * sin (rad (geomMeanAnom cent))
   + 4 * eccOrbit cent * yVar cent * sin (rad (geomMeanAnom cent)) * cos (2*rad (geomMeanLong cent))
   - 0.5 * yVar cent * yVar cent * sin (4 * rad (geomMeanLong cent))
   - 1.25 * eccOrbit cent * eccOrbit cent * sin (2 * rad (geomMeanAnom cent)))


haSunrise, sunlightDuration :: (Floating a, RealFrac a) => a -> a -> a
haSunrise cent lat =
     deg (acos (cos (rad 90.833)/(cos (rad lat) * cos (rad (sunDeclin cent)))
   - tan (rad lat) * tan (rad (sunDeclin cent))))


-- | Sunrise given in local solar time.
sunriseLST, sunsetLST :: (Floating a, RealFrac a) => a -> a -> a -> a -> a
sunriseLST cent tz lat long = solarNoonLST cent tz long - (4 * haSunrise cent lat)

-- | Solar Noon given in local solar time.
-- minutes since midnight local time 00:00 
solarNoonLST :: (Floating a, RealFrac a) => a -> a -> a -> a
solarNoonLST cent tz long =
    720 - 4 * long - eqTime cent + tz * 60

-- | Sunset given in local solar time.
sunsetLST cent tz lat long = solarNoonLST cent tz long  + haSunrise cent lat * 4

sunlightDuration cent lat = 8 * haSunrise cent lat

trueSolarTime, hourAngle :: Double -> Double -> Double -> Double -> Double
trueSolarTime tcurrent tz cent longit =
   nonIntRem ( tcurrent + eqTime cent + 4 * longit - 60*tz) 1440


hourAngle tcurrent tz cent longit
    | tst < 0 = tst + 180
    | otherwise = tst - 180
  where
    tst = trueSolarTime tcurrent tz cent longit / 4

solarZenithAngle, solarElevationAngle, atmosRefract, refractCorrectElevation
 :: Double -> Double -> Double -> Double -> Double -> Double
solarZenithAngle tcurrent tz cent lat longit =
    let sins = sin (rad  lat ) * sin (rad  (sunDeclin cent))
        coss =
            cos (rad  lat ) * cos (rad  (sunDeclin cent)) *
            cos (rad (hourAngle tcurrent tz cent longit))
     in deg . acos $ sins + coss


solarElevationAngle tcurrent tz cent lat longit =
     90 - solarZenithAngle tcurrent tz cent lat longit


atmosRefract tcurrent tz cent lat longit =
    let h = solarElevationAngle tcurrent tz cent lat longit
        belowZero h = (- 20.774) / tan (rad h) / 3600
        belowFive h =
          (1735 - 518.2 * h + 103.4 * h ^ 2 - 12.79 * h ^ 3 + 0.711 * h ^ 4) / 3600
        belowEightyFive h =
           ( 58.1 / tan (rad h) - 0.07/tan (rad h)^3 + 8.6e-5/tan (rad h)^5) / 3600
    in
      if h < -0.575 then belowZero h
      else if h <= 5.0 then belowFive h
      else if h <= 85.0 then belowEightyFive h
      else  0


refractCorrectElevation tcurrent tz cent lat longit =
    solarElevationAngle tcurrent tz cent lat longit + atmosRefract tcurrent tz cent lat longit

-- About atmosperic refraction:
-- https://gml.noaa.gov/grad/solcalc/calcdetails.html

solAzimuth :: (RealFrac a1, Ord a2, Floating a1, Fractional a2) => a2 -> a1 -> a1 -> a1 -> a1
solAzimuth hrA lat solZen sunDecl=
    let
        preAz =
            deg preAzimuth

        ac =
            hrA
    in
    if ac > 0.0 then
         nonIntRem (preAz + 180.0) 360

    else
         nonIntRem (540.0 - preAz) 360
    where
    preAzimuth  =
        let
        b3 =
            rad lat

        ad =
            rad solZen
        t =
            rad sunDecl
        in
        acos ((sin b3 * cos ad - sin t) / (cos b3 * sin ad))


showLocalTime hr mn sc =
    let h1 = truncate hr
        h2 = if h1 > 24 then h1 -  24
        else h1
        mn2 = truncate mn
        sc2 = round sc
        show2 x = 
           if x < 10 then "0" ++ show x
           else show x
    in show2 h2 ++ ":" ++ show2 mn2 ++ ":" ++ show2 sc2


showtime :: RealFrac p => p -> [Char]
showtime xmn =
   let h = truncate (xmn / 60)
       z = xmn - fromIntegral (60 * h)
       mins = truncate z
       dmins = z - fromIntegral mins
       secs = round (60*dmins)
       show2 x =
         if x < 10 then "0" ++ show x
         else show x
    in   show2 h ++ ":" ++ show2 mins ++ ":" ++ show2 secs


isLeapYear y
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

{-
fromPOSIX t =
    read $ show t :: Double


toPOSIX t =
    let ts = show t
    in  read ts :: POSIXTime

fromUTC ut =
    let uts = show ut 
        ls = length uts - 1
        utc = take ls uts
    in  read utc :: Double

toDouble pt =
    read $ show pt :: Double

-}    

-- modulo for real numbers

nonIntRem :: RealFrac a => a -> a -> a
nonIntRem x y = x - (y * fromIntegral (truncate (x/y)))

-- Converts the date (year, month, day) to string

dateString :: Int -> Int -> Int -> String
dateString y m d =
   show y ++ "-"
      ++ show2 m ++ "-"
      ++ show2 d :: String
      where
        show2 x =
         if x < 10 then "0" ++ show x
         else show x

getSeconds :: Integral b => UTCTime -> b
getSeconds ct =
    let posixSeconds = utcTimeToPOSIXSeconds ct
    in  round posixSeconds

-- Try with Haskell play https://play.haskell.org/saved/VgnWF4d5 
