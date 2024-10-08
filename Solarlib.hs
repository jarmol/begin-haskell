module Solarlib where

-- | This version calculates solar position and events for current date and fixed times of day
-- and fixed location.
-- Sunrise, noon and Sunset is calculated using all around the year the normal time, i.e. without DLST (summer time).


-- | Declaring the operator '//' to be used instead of div
-- (//) = div
-- infixl 8  //

(//) :: Int -> Int -> Int
(//) = posit
infixl 8 //

-- | Function for Julian day number JDN calculated from the given date
-- where y = year, m = month, d = day

posit :: Integral a => a -> a -> a
posit h k =
     if h >= 0 then (div h k)
     else  negate (div (-h) k)

-- | Used in calculation of Julian date
jdnGr :: Int -> Int -> Int -> Int
jdnGr y m d = (1461 * (y + 4800 + (m - 14)//12))//4
        + (367 * (m - 2 - 12 * ((m - 14)//12)))//12
        - ((3 * ((y + 4900 + (m - 14)//12)//100))//4)
        + d - 32075


julianDay :: Int -> Int -> Int -> Double -> Double -> Double
julianDay y m d tz tloc =
    fromIntegral (jdnGr y m d) + (tloc - tz) / 24 - 0.5


julianCentury :: Int -> Int -> Int -> Double -> Double -> Double
julianCentury y m d tz tloc =
    ( julianDay y m d tz tloc - 2451545 ) / 36525

-- | Geom. average sun longitude
geomMeanLong :: Int -> Int -> Int -> Double -> Double -> Double
geomMeanLong y m d tz tloc =
    let jC = julianCentury y m d tz tloc
    in  nonIntRem (280.46646 + (jC * (36000.76983 + jC * 0.0003032))) 360


geomMeanAnom y m d tz tloc =
    let jC = julianCentury y m d tz tloc
    in  nonIntRem (357.52911 + jC * (35999.05029 - 0.0001537 * jC)) 360



eccOrbit y m d tz tloc =
    let cent = julianCentury y m d tz tloc
    in 0.016708634 - cent * (0.000042037 + 0.0000001267 * cent)


sunEqOfCtr y m d tz tloc =
  let gA = geomMeanAnom y m d tz tloc
      jC = julianCentury y m d tz tloc
  in  sin (  rad gA ) * ( 1.914602 - jC * ( 0.004817 + 0.000014 * jC ))
      + sin ( rad  2 * gA ) * ( 0.019993 - 0.000101 * jC )
      + sin ( rad  3 * gA ) * 0.000289

rad :: Floating a => a -> a
rad g = pi*g/180.0

deg :: Floating a => a -> a
deg r = 180.0 * r / pi

sunTrueLong :: Int -> Int -> Int -> Double -> Double -> Double
sunTrueLong y m d tz tloc =
    geomMeanLong y m d tz tloc + sunEqOfCtr y m d tz tloc

sunTrueAnom :: Int -> Int -> Int -> Double -> Double -> Double
sunTrueAnom y m d tz tloc =
    nonIntRem (geomMeanAnom y m d tz tloc + sunEqOfCtr y m d tz tloc) 360


sunAppLong :: Int -> Int -> Int -> Double -> Double -> Double
sunAppLong y m d tz tloc =
    sunTrueLong y m d tz tloc - 0.00569 -
    0.00478 * sin ( rad (125.04 - 1934.136 * julianCentury y m d tz tloc))


-- | deg

meanObliqEcliptic :: Int -> Int -> Int -> Double -> Double -> Double
meanObliqEcliptic y m d tz tloc =
    let cent = julianCentury y m d tz tloc
     in 23 +
        (26 +
         (21.448 - cent * (46.815 + cent * (0.00059 - cent * 0.001813))) / 60) /
        60


obliqCorr y m d tz tloc =
    meanObliqEcliptic y m d tz tloc
     + 0.00256 * cos (rad (125.04 - 1934.136 * julianCentury y m d tz tloc))

-- | Solar declination angle (deg)

sunDeclin y m d tz tloc =
    deg . asin $
    sin (rad $ obliqCorr y m d tz tloc) * sin (rad $ sunAppLong y m d tz tloc)


-- | Y-variable
yVar y m d tz tloc = tan (rad (obliqCorr y m d tz tloc / 2)) * tan (rad (obliqCorr y m d tz tloc/2))


-- | Equation of time
eqTime y m d tz tloc =
     4 * deg (yVar y m d tz tloc * sin (2 * rad (geomMeanLong y m d tz tloc))
   - 2 * eccOrbit y m d tz tloc * sin (rad (geomMeanAnom y m d tz tloc))
   + 4 * eccOrbit y m d tz tloc * yVar y m d tz tloc * sin (rad (geomMeanAnom y m d tz tloc)) * cos (2*rad (geomMeanLong y m d tz tloc))
   - 0.5 * yVar y m d tz tloc * yVar  y m d tz tloc * sin (4 * rad (geomMeanLong y m d tz tloc))
   - 1.25 * eccOrbit y m d tz tloc * eccOrbit y m d tz tloc * sin (2 * rad (geomMeanAnom y m d tz tloc)))

latitude :: Double
latitude = 65.85
longitude = 24.18

haSunrise :: Int -> Int -> Int -> Double -> Double -> Double
haSunrise y m d tz tloc =
     deg (acos (cos (rad 90.833)/(cos (rad latitude) * cos (rad (sunDeclin y m d tz tloc)))
   - tan (rad latitude) * tan (rad (sunDeclin y m d tz tloc))))


-- | Solar Noon given in local solar time.
-- minutes since midnight local time 00:00 

solarNoonLST :: Int -> Int -> Int -> Double -> Double -> Double
solarNoonLST y m d tz tloc =
    720 - 4 * longitude - eqTime y m d tz tloc + tz * 60

-- | Sunrise in local time.

sunriseLST :: Int -> Int -> Int -> Double -> Double -> Double
sunriseLST y m d tz tloc = solarNoonLST y m d tz tloc - haSunrise y m d tz tloc * 4

-- | Sunset in local time.
sunsetLST :: Int -> Int -> Int -> Double -> Double -> Double
sunsetLST y m d tz tloc = solarNoonLST y m d tz tloc  + haSunrise y m d tz tloc * 4

-- | Duration of sunlight on a given date and location.
sunlightDuration :: Int -> Int -> Int -> Double -> Double -> Double
sunlightDuration y m d tz tloc = 8 * haSunrise y m d tz tloc

trueSolarTime :: Int -> Int -> Int -> Double -> Double -> Double
trueSolarTime y m d tz tloc =
   nonIntRem (60 * tloc + eqTime y m d tz tloc + 4 * longitude - 60*tz) 1440


hourAngle :: Int -> Int -> Int -> Double -> Double -> Double
hourAngle y m d tz tloc
    | tst < 0 = tst + 180
    | otherwise = tst - 180
  where
    tst = trueSolarTime y m d tz tloc / 4


solarZenithAngle :: Int -> Int -> Int -> Double -> Double -> Double
solarZenithAngle y m d tz tloc =
    let sins = sin (rad  latitude ) * sin (rad  (sunDeclin y m d tz tloc))
        coss =
            cos (rad  latitude ) * cos (rad  (sunDeclin y m d tz tloc)) *
            cos (rad (hourAngle y m d tz tloc))
     in deg . acos $ sins + coss


solarElevationAngle :: Int -> Int -> Int -> Double -> Double -> Double
solarElevationAngle y m d tz tloc = 90 - solarZenithAngle y m d tz tloc

atmosRefract h
  | h < -0.575 = belowZero h
  | h <= 5.0 = belowFive h
  | otherwise = 0
  where
      belowZero h = (- 20.774) / tan (rad h) / 3600
      belowFive h
        = (1735 - 518.2 * h + 103.4 * h ^ 2 - 12.79 * h ^ 3 + 0.711 * h ^ 4) / 3600

-- | About atmosperic refraction:
--   see https://gml.noaa.gov/grad/solcalc/calcdetails.html

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



-- | Definition of the leapyear
isLeapYear y
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False


-- | Function defines the weekday from JDN

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
            "Tuesday(367"
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


-- | modulo function for real numbers

nonIntRem :: RealFrac a => a -> a -> a
nonIntRem x y = x - (y * fromIntegral (truncate (x/y)))

-- | Converts the date (year, month, day) to string

dateString :: Int -> Int -> Int -> String
dateString y m d =
   show y ++ "-"
      ++ show m ++ "-"
      ++ show d :: String


-- | testing
-- main = jdnGr 2023 2 13  -- 2459989
-- Try with Haskell play https://play.haskell.org/saved/VgnWF4d5 
