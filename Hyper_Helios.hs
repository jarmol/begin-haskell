module Main

where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime )
import Data.Time ( toGregorian, UTCTime(utctDay), getCurrentTime, utcToLocalTimeOfDay )
import Numeric ( showFFloat )
import SolarCurrent
    (julianCentury
    , sunDeclin
    , solarNoonLST
    , sunriseLST
    , sunsetLST
    , showtime
    , solarZenithAngle
    , atmosRefract
    , solAzimuth, hourAngle, sunlightDuration
    , nonIntRem
    , showLocalTime
 )


-- General data:  latitude & longitude
latitude :: Double
latitude = 65.85

longitude :: Double
longitude = 24.18


timeZone = 2.0

main :: IO ()
main = do
    currentTime <- getCurrentTime
    let (year, month, day) = toGregorian . utctDay $ currentTime
        ps = utcTimeToPOSIXSeconds currentTime
        hours = nonIntRem (ps / 3600) 24
        minutes = nonIntRem (ps / 60) 60
        seconds =  nonIntRem ps 60
        lochour = hours +  timeZone
        posMinutes = 60 * lochour
        decMinutes = 0.001 * fromInteger (round $ 1000 * posMinutes + 5)
        jC = julianCentury currentTime
        sunDecl = sunDeclin jC
        sunRise = sunriseLST jC 2.0 latitude longitude
        solNoon = solarNoonLST jC 2.0 longitude
        sunSet = sunsetLST jC 2.0 latitude longitude
        sunLD = sunlightDuration jC latitude
        hrAngle = hourAngle decMinutes 2.0  jC longitude
        solZen = solarZenithAngle decMinutes 2.0 jC latitude longitude
        solElevat = 90.0 - solZen
        refract = atmosRefract decMinutes 2.0 jC latitude longitude
        solAz = solAzimuth hrAngle latitude solZen sunDecl
        (blue, green, red, yellow, white, black, bluebg, whitebg,blackbg) = ("\ESC[94m","\ESC[92m","\ESC[91m","\ESC[93m","\ESC[97m","\ESC[30m","\ESC[44m","\ESC[107m","\ESC[40m" )
    putStrLn (white ++ bluebg)
    putStrLn "  OBSERVATION LOCATION  "
    putStrLn $ "  Latitude: " ++ show latitude
     ++ "  Longitude: " ++ show longitude
      ++ " Timezone: " ++ show (truncate timeZone)
    putStrLn $ "  Date and Current Time " ++ show currentTime
    putStrLn $ "  Local time  " ++ showLocalTime lochour minutes seconds 
       ++ " ( UTC + " ++ show (truncate timeZone) ++ " h )"
    putStrLn $ "  Julian Century " ++ showFFloat (Just 8) jC ""
    putStrLn (yellow ++ blackbg ++  "\n  CURRENT SOLAR POSITION")
    putStrLn $ "  Sun Declination "       ++ showFFloat (Just 4) sunDecl "°"
    putStrLn $ "  Sunrise    " ++ showtime sunRise
    putStrLn $ "  Solar Noon " ++ showtime solNoon
    putStrLn $ "  Sunset     " ++ showtime sunSet
    putStrLn $ "  Sunlight duration " ++ showtime sunLD
    putStrLn $ "  Solar Zenith angle " ++ showFFloat (Just 4) solZen  "°"
    putStrLn $ "  Solar elevation angle " ++ showFFloat (Just 4) solElevat  "°"
    putStrLn $ "  Atmospheric refraction " ++ showFFloat (Just 4) refract  "°"
    putStrLn $ "  Refraction corrected elevation " ++ showFFloat (Just 4) (solElevat + refract) "°"
    putStrLn $ "  Solar Azimuth angle " ++ showFFloat (Just 3) solAz  "°"
    putStrLn (green ++ ".")

    -- About colours https://i.sstatic.net/9UVnC.png
