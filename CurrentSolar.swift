import Foundation

print("Enter location values!")

let array = readLine()?
    .split { $0 == " " }
    .map(String.init)

let now = Date()
if let stringArray = array {
    if stringArray.count != 4 {
        print(
            """
            This can be run on line at 
            https://www.mycompiler.io/view/9T2OZ6mG4aL
            Enter city name and three numeric values!
            Your location name, what ever,
            Latitude angle min -90 S max 90 N.
            Minus is south from equator and plus is north.
            Do not enter values with S or N
            Longitude angle min -180 W max 180 E
            Minus sign is for western longitudes.
            Do not enter W or E, give just the numbers with
            right signs.
            Time zone hours min -12 W max 12 E.
            Note, increase time zone by one hour for summer times.

            Use example, enter
            NewYork 40.713 -74.0 -4.0
            New York is set default. So you get the default values 
            entering non-numeric values, example like
            What_ever_place a b c
            Note, the city name must be one word, which ever.  
            More examples, enter following...
            Helsinki 60.168 24.953 3.0  (Summertime)
            Stockholm 59.328 18.053 2.0 (Summertimei)
            Berlin 52.519 13.408 2.0    (Summertime)
            Hamburg 53.551 9.993 2.0    (Summertime)
            Murcia 37.983 -1.130 2.0    (Summertime)   
            For comparison of results, look for example at
            https://www.timeanddate.com/sun/finland/helsinki
            https://www.timeanddate.com/sun/spain/murcia
            """)
    } else {
        print(stringArray)
        print("City " + stringArray[0])
        let lat = Double(stringArray[1]) ?? 40.713
        let lon = Double(stringArray[2]) ?? -74.0
        let tz = Double(stringArray[3]) ?? -4.0
        print("Latitude \(lat) °")
        print("Longitude \(lon) °")
        print("Time Zone \(tz) hours")
        print("Date and time now \(now)")
        //  print("Posix seconds \(now.timeIntervalSince1970)")

        let posixDays = Double(now.timeIntervalSince1970) / (3600 * 24)
        //  let julianDay = posixDays + 2_440_587
        let tcurrent: Double = 1440 * (posixDays - Double(Int(posixDays)))
        //  print("Julian Day   \(julianDay)")

        let jCent = julianCentury(epochDays: posixDays)

        //  print("Posix Days     \(posixDays)")
        //  print("Julian Century \(jCent)")
        print("________________")
        print(String(format: "Sun declination angle   %8.5f °", sunDeclin(cent: jCent)))

        print(
            "Sunrise local time:    "
                + clocktimeFromMinutes(
                    rawminutes: sunriseLST(cent: jCent, tz: tz, lat: lat, lon: lon)))
        print(
            "Solar noon time:      "
                + clocktimeFromMinutes(
                    rawminutes: solarNoonLST(cent: jCent, tz: tz, lat: lat, lon: lon)))
        print(
            "Sunset local time:    "
                + clocktimeFromMinutes(
                    rawminutes: sunsetLST(cent: jCent, tz: tz, lat: lat, lon: lon)))
        print("Daylight duration     " + sunlightDuration(cent: jCent, lat: lat))

        let zen = solarZenithAngle(tcurrent: tcurrent, cent: jCent, lat: lat, lon: lon)
        let el1 = 90.0 - zen
        let refr = atmosRefract(tcurrent: tcurrent, cent: jCent, lat: lat, lon: lon)
        print(String(format: "Solar elevation angle   %7.4f °", el1))
        print(String(format: "Atmospheric refraction  %8.5f °", refr))
        print(String(format: "Sol. elev, refr. corr. %8.4f °", (el1 + refr)))

        let hrA = hourAngle(tcurrent: tcurrent, cent: jCent, lon: lon)
        let sunDecl = sunDeclin(cent: jCent)
        let result: Double = calcAzimuth(
            hourAngle: hrA, zenith: zen, sunDeclin: sunDecl, latit: lat)
        let azimuthS = String(format: "Azimuth angle  *)    %9.3f °", result)
        print(azimuthS)
        print("_________________________")
        print(
            """
            *) Azimuth angle is the direction where the
               Sun is seen on the current time:
               N =   0°
               E =  90°
               S = 180°
               W = 270°
               N = 360°
            """)
    }
}

func julianCentury(epochDays: Double) -> Double {
    let numberJD = 2440587.5 + epochDays
    return (numberJD - 2451545.0) / 36525.0
}

func geomMeanLong(cent: Double) -> Double {
    let geomMeanLong = nonIntRem(
        x: (280.46646 + (cent * (36000.76983 + cent * 0.0003032))), y: 360.0)
    return geomMeanLong
}

func nonIntRem(x: Double, y: Double) -> Double { x - (y * Double(floor(x / y))) }

func eccOrbit(cent: Double) -> Double {
    0.016708634 - cent * (0.000042037 + 0.0000001267 * cent)
}

func geomMeanAnom(cent: Double) -> Double {
    let geomMeanAnom = nonIntRem(
        x: (357.52911 + cent * (35999.05029 - 0.0001537 * cent)), y: 360.0)
    return geomMeanAnom
}

func sunEqOfCtr(cent: Double) -> Double {
    let gA = geomMeanAnom(cent: cent)
    let sunEqOfCtr =
        sin(rad(g: gA)) * (1.914602 - cent * (0.004817 + 0.000014 * cent))
        + sin(rad(g: 2.0 * gA)) * (0.019993 - 0.000101 * cent)
        + sin(rad(g: 3.0 * gA)) * 0.000289
    return sunEqOfCtr
}

func sunAppLong(cent: Double) -> Double {
    // sunTL is Sun true longitude composed here
    let sunTL = sunEqOfCtr(cent: cent) + geomMeanLong(cent: cent)
    let sunAppLong = sunTL - 0.00569 - 0.00478 * sin(rad(g: (125.04 - 1934.136 * cent)))
    return sunAppLong
}

func meanObliqEcliptic(cent: Double) -> Double {
    23 + (26 + (21.448 - cent * (46.815 + cent * (0.00059 - cent * 0.001813))) / 60) / 60
}

func obliqCorr(cent: Double) -> Double {
    meanObliqEcliptic(cent: cent) + 0.00256 * cos(rad(g: 125.04 - 1934.136 * cent))
}

// Sun declination angle
func sunDeclin(cent: Double) -> Double {
    deg(r: (asin(sin(rad(g: obliqCorr(cent: cent))) * sin(rad(g: sunAppLong(cent: cent))))))
}

// Y-variable
func yVar(cent: Double) -> Double {
    return tan(rad(g: obliqCorr(cent: cent)) / 2.0) * tan(rad(g: obliqCorr(cent: cent)) / 2.0)
}

// Equation of time
func eqTime(cent: Double) -> Double {
    let gA: Double = geomMeanAnom(cent: cent)
    let gL: Double = geomMeanLong(cent: cent)
    let eO: Double = eccOrbit(cent: cent)
    let y: Double = yVar(cent: cent)
    let v1: Double = y * sin(2.0 * rad(g: gL))
    let v2: Double = sin(rad(g: gA))
    let eqTime: Double =
        4.0
        * deg(
            r: (v1 - 2.0 * eO * v2
                + 4.0 * eO * y * v2 * cos(2.0 * rad(g: gL))
                - 0.5 * y * y * sin(4.0 * rad(g: gL))
                - 1.25 * eO * eO * sin(2.0 * rad(g: gA))))
    return eqTime
}

func haSunrise(cent: Double, lat: Double) -> Double {
    return deg(
        r: acos(
            cos(rad(g: 90.833))
                / (cos(rad(g: lat))
                    * cos(rad(g: sunDeclin(cent: cent))))
                - tan(rad(g: lat)) * tan(rad(g: sunDeclin(cent: cent)))))
}

// Sunrise given in local solar time.
func sunriseLST(cent: Double, tz: Double, lat: Double, lon: Double) -> Double {
    solarNoonLST(cent: cent, tz: tz, lat: lat, lon: lon) - 4.0 * haSunrise(cent: cent, lat: lat)
}

func solarNoonLST(cent: Double, tz: Double, lat: Double, lon: Double) -> Double {
    720.0 - 4.0 * lon - (eqTime(cent: cent)) + tz * 60.0
}

// Sunset given in local solar time.
func sunsetLST(cent: Double, tz: Double, lat: Double, lon: Double) -> Double {
    solarNoonLST(cent: cent, tz: tz, lat: lat, lon: lon) + 4.0 * haSunrise(cent: cent, lat: lat)
}

// Sunlight duration
func sunlightDuration(cent: Double, lat: Double) -> String {
    let mins = 8 * haSunrise(cent: cent, lat: lat)
    return clocktimeFromMinutes(rawminutes: mins)
}

func clocktimeFromMinutes(rawminutes: Double) -> String {
    // Converts minutes to normal clocktime
    let rawhours: Double = rawminutes / 60.0
    let rawseconds: Double = 60.0 * rawminutes

    let hours = Int(rawhours)
    let minutes = Int(rawminutes) % 60
    let seconds = Int(rawseconds) % 60

    return "  " + String(hours) + ":" + String(minutes) + ":" + String(seconds)
}

func trueSolarTime(tcurrent: Double, cent: Double, lon: Double) -> Double {
    tcurrent + eqTime(cent: cent) + 4.0 * lon
}

func hourAngle(tcurrent: Double, cent: Double, lon: Double) -> Double {
    let tst: Double = trueSolarTime(tcurrent: tcurrent, cent: cent, lon: lon) / 4.0
    var res: Double = tst
    if tst < 0 { res = tst + 180.0 }
    if tst > 0 { res = tst - 180.0 }
    return res
}

func solarZenithAngle(tcurrent: Double, cent: Double, lat: Double, lon: Double) -> Double {
    let sins: Double = sin(rad(g: lat)) * sin(rad(g: sunDeclin(cent: cent)))
    let coss: Double =
        cos(rad(g: lat)) * cos(rad(g: sunDeclin(cent: cent)))
        * cos(rad(g: hourAngle(tcurrent: tcurrent, cent: cent, lon: lon)))
    return deg(r: acos(sins + coss))
}

func atmosRefract(tcurrent: Double, cent: Double, lat: Double, lon: Double) -> Double {
    let h = 90.0 - solarZenithAngle(tcurrent: tcurrent, cent: cent, lat: lat, lon: lon)

    let res =
        if h < -0.575 { belowZero(hx: h) } else if h <= 5.0 {
            belowFive(hx: h)
        } else if h <= 85.0 {
            belowEightyFive(hx: h)
        } else { 0.0 }

    return res
}

func belowZero(hx: Double) -> Double {
    return -20.774 / tan(rad(g: hx)) / 3600.0
}

func belowEightyFive(hx: Double) -> Double {
    let v1: Double = tan(rad(g: hx))
    let v2: Double = pow(tan(rad(g: hx)), 3.0)
    let v3: Double = pow(tan(rad(g: hx)), 5.0)
    let v: Double = ((58.1 / v1) - (0.07 / v2) + (8.6e-5 / v3)) / 3600.0

    return v
}

func belowFive(hx: Double) -> Double {
    let v: Double =
        (1735.0 - 518.2 * hx + 103.4 * pow(hx, 2.0)
            - 12.79 * pow(hx, 3.0) + 0.711 * pow(hx, 4.0)) / 3600.0

    return v
}

func calcAzimuth(hourAngle: Double, zenith: Double, sunDeclin: Double, latit: Double) -> Double {
    let radZenith = rad(g: zenith)
    let radLatit = rad(g: latit)
    let radS = rad(g: sunDeclin)

    let numerator = sin(radLatit) * cos(radZenith) - sin(radS)
    let denominator = cos(radLatit) * sin(radZenith)

    let acosValue = acos(numerator / denominator)
    let degreesValue = acosValue * 180 / .pi
    if hourAngle > 0 {
        return fmod(degreesValue + 180, 360)
    } else {
        return fmod(540 - degreesValue, 360)
    }
}

func rad(g: Double) -> Double {
    .pi * g / 180.0
}

func deg(r: Double) -> Double {
    180.0 * r / .pi
}
