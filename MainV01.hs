module MainV01  (main) where

import Solarlib (
    dateString
   , julianDay
   , julianCentury
   , sunDeclin 
   , solarNoonLST
   , showtime
   , sunriseLST
   , sunsetLST
   , sunlightDuration
   , latitude
   , solarElevationAngle
   , atmosRefract
   , refractCorrectElevation
   , solAzimuth
   , longitude )

import Numeric ( showFFloat )
import Text.Printf ( printf )
import Data.Time (toGregorian, getCurrentTime, UTCTime(utctDay))
import Data.Maybe (fromJust)
import Data.Char (GeneralCategory(DecimalNumber))

-- | This version calculates solar position and events for current date and fixed times of day
-- and fixed location.
-- Sunrise, noon and Sunset is calculated using all around the year the normal time, i.e. without DLST (summer time).

putsRow :: Int -> Int -> Int -> Double -> Double -> IO ()
putsRow y m d tz tloc =
  do
    let jD = julianDay y m d tz tloc
        jC = julianCentury y m d tz tloc
-- | Noon in local time
        noonT = solarNoonLST y m d tz tloc
-- | Sunrise in local time
        sunriseT = sunriseLST y m d tz tloc
-- | Sun declination degrees
        sunDec = sunDeclin y m d tz tloc
-- | Sunset in local time.
        sunsetT = sunsetLST y m d tz tloc
-- | Daylength in minutes
        dayL    = sunlightDuration y m d tz tloc
        atmosR  = atmosRefract y m d tz tloc
        refrCE = refractCorrectElevation y m d tz tloc
        solAz = solAzimuth y m d tz tloc
-- | Current date
    putStr (dateString y m d)
-- | UTC time = local time - time zone in hours
    printf "%6.2f" (tloc - tz)
-- | All around the year time zone constant, ie. normal time without daylight saving times
    printf "%7.2f " tloc
    printf "%11.3f  " jD
    printf "%8.4f" sunDec
    printf "%10s" (showtime sunriseT)
    printf "%10s" (showtime noonT)
    printf "%10s" (showtime sunsetT)
    printf "%10s" (showtime dayL)
    printf "%10.5f" atmosR
    printf "%12.5f" refrCE
    printf "%10.3f\n" solAz



main :: IO ()
main = do
    printf "%s:%6.2f° %s:%6.2f°\n" "Latitude " latitude "  Longitude " longitude
    currentTime <- getCurrentTime
    print currentTime

    putStr "\nDate       UTC    Local  Julian     "
    putStrLn "  Sun      Sunrise   Noon      Sunset    Sunlight   Atmospher   Solar     Solar"
    putStr "           time   time   day          "
    putStrLn "Declinat time      time      time      Duration   corr. refr  Elevation Azimuth"
    let  (year, month, day) = toGregorian . utctDay $ currentTime
         y0 = read (show year) :: Int
         m0 = read (show month) :: Int
         d0 = read (show day) :: Int
    putsRow y0 m0 d0 2.0  2.0
    putsRow y0 m0 d0 2.0  4.5
    putsRow y0 m0 d0 2.0  8.0
    putsRow y0 m0 d0 2.0 12.0
    putsRow y0 m0 d0 2.0 14.0
    putsRow y0 m0 d0 2.0 20.0
