module Main 
where
    
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time ( toGregorian, UTCTime(utctDay), getCurrentTime )
import Numeric ( showFFloat, showEFloat )
import SolarCurrent (julianCentury
    , sunDeclin
    , solarNoonLST
    , sunriseLST
    , sunsetLST
    , showtime
    , solarZenithAngle
    , atmosRefract
    , solAzimuth, hourAngle
 )

getSeconds :: Integral b => UTCTime -> b
getSeconds ct =
    let posixSeconds = utcTimeToPOSIXSeconds ct
    in  round posixSeconds

-- General data: time zone, latitude & longitude
timeZone :: Double
timeZone = 2

latitude :: Double
latitude = 65.85

longitude :: Double
longitude = 24.18

main :: IO ()
main = do
    currentTime <- getCurrentTime
    let (year, month, day) = toGregorian . utctDay $ currentTime
        ps = round $ utcTimeToPOSIXSeconds currentTime
        hours = read $ show $ mod (div ps 3600) 24
        minutes = read $ show $ mod (div ps 60) 60
        seconds = read $ show $ mod ps 60
        jC = julianCentury currentTime
        sunDecl = sunDeclin jC
        solNoon = solarNoonLST jC 2.0 longitude
        sunRise = sunriseLST jC 2.0 latitude longitude
        sunSet = sunsetLST jC 2.0 latitude longitude
        lochour = hours + timeZone
        hrAngle = hourAngle posMinutes 2.0  jC longitude
        posMinutes =  (60*lochour + minutes) + seconds/60.0
        solZen = solarZenithAngle posMinutes 2.0 jC latitude longitude
        solElevat = 90.0 - solZen
        refract = atmosRefract posMinutes timeZone jC latitude longitude
        solAz = solAzimuth hrAngle latitude solZen sunDecl
        (blue, green, red, yellow, white, black, bluebg, whitebg,blackbg) = ("\ESC[94m","\ESC[92m","\ESC[91m","\ESC[93m","\ESC[97m","\ESC[30m","\ESC[44m","\ESC[107m","\ESC[40m" )
    putStrLn (white ++ bluebg)
    putStrLn "  OBSERVATION LOCATION  "
    putStrLn ("  Latitude: " ++ show latitude ++ " . Longitude: " ++ show longitude ++ " Timezone: " ++ show timeZone)
    putStr "  Date and Current Time "
    putStrLn $ formatTime defaultTimeLocale "%y-%m-%d %H:%M:%S" currentTime
    putStrLn $ "  Local time  " ++ showtime posMinutes ++ " UTC + " ++ show timeZone ++ " h"
    putStrLn (yellow ++ blackbg ++  "\n  CURRENT SOLAR POSITION")
    putStrLn $ "  Sun Declination "       ++ showFFloat (Just 4) sunDecl "°"
    putStrLn $ "  Sunrise    " ++ showtime sunRise
    putStrLn $ "  Solar Noon " ++ showtime solNoon
    putStrLn $ "  Sunset     " ++ showtime sunSet
    putStrLn $ "  Solar elevation angle " ++ showFFloat (Just 4) solElevat  "°"
    putStrLn $ "  Atmospheric refraction " ++ showFFloat (Just 4) refract  "°"
    putStrLn $ "  Refraction corrected elevation " ++ showFFloat (Just 4) (solElevat + refract) "°" 
    putStrLn $ "  Solar Azimuth angle " ++ showFFloat (Just 3) solAz  "°" 
    putStrLn (black ++ whitebg ++ ".")
    -- About colours https://i.sstatic.net/9UVnC.png
