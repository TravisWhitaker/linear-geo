{-|
Module      : Linear.Geo.Time
Copyright   : Travis Whitaker 2025
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

Types for dealing with time quantities relevant to geographical or celestial
coordinate systems.

-}

{-# LANGUAGE DeriveGeneric
           , DerivingStrategies
           , ScopedTypeVariables
           #-}

module Linear.Geo.Time (
    utcToUT1
  , ut1ToUTC
  , JulianDate(..)
  , julianDateFromUniversalTime
  , universalTimeFromJulianDate
  , TT(..)
  , ttFromTAI
  , taiFromTT
  , GMST(..)
  , gmstFromJulianDate
  , getCurrentUT1
  , getCurrentJulianDate
  , getCurrentGMST
  , getCurrentTAI
  , getCurrentTT
  ) where

import Data.Fixed

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.TAI

import GHC.Generics

import Linear.Geo.PlaneAngle

posixDayLength :: Num a => a
posixDayLength = 86400

-- | UT1 - UTC = DUT in seconds
type DUT = DiffTime

-- | Unclear if this works if a leap second occurs at this exact time.
utcToUT1 :: DUT -> UTCTime -> UniversalTime
utcToUT1 ut1MinusUTC (UTCTime (ModifiedJulianDay mjDays) tod) =
    ModJulianDate (fromIntegral mjDays + toRational (tod + ut1MinusUTC) / posixDayLength)


-- | Unclear if this works if a leap second occurs at this exact time.
ut1ToUTC :: DUT -> UniversalTime -> UTCTime
ut1ToUTC ut1MinusUTC (ModJulianDate mjd) =
    let mjDays = floor mjd
        todFrac = fromRational (mjd - toRational mjDays)
    in UTCTime (ModifiedJulianDay mjDays) ((todFrac * 86400) - ut1MinusUTC)

modJulDateEpoch :: Rational
modJulDateEpoch = 2400000.5

-- | Fractional Julian days since noon UT1 on Monday, January 1, 4713 BC
--   (November 24, 4717 BC Gregorian).
newtype JulianDate = JulianDate { getJulianDate :: Rational }
                   deriving stock ( Generic
                                  , Eq
                                  , Ord
                                  )

-- | Show via `UniversalTime`
instance Show JulianDate where
    showsPrec p jd = showsPrec p (universalTimeFromJulianDate jd)

julianDateFromUniversalTime :: UniversalTime -> JulianDate
julianDateFromUniversalTime (ModJulianDate mjd) = JulianDate (mjd + modJulDateEpoch)

universalTimeFromJulianDate :: JulianDate -> UniversalTime
universalTimeFromJulianDate (JulianDate jd) = ModJulianDate (jd - modJulDateEpoch)

-- | Terrestrial time, used to be called "terrestrial dynamical time," successor
--   of ephemeris time (ET).
newtype TT = TT { ttSeconds :: DiffTime }

ttToTAI :: Fractional a => a
ttToTAI = 32.184

ttFromTAI :: AbsoluteTime -> TT
ttFromTAI tai = TT $ diffAbsoluteTime tai taiEpoch + ttToTAI

taiFromTT :: TT -> AbsoluteTime
taiFromTT (TT t) = addAbsoluteTime (t - ttToTAI) taiEpoch

-- | Mean sidereal time at the prime meridian.
newtype GMST a = GMST (Radians a)

-- | Based on Spacetrak3 Vallado
gmstFromJulianDate :: forall a. (Floating a, Real a) => JulianDate -> GMST a
gmstFromJulianDate (JulianDate jdut1) = let
    deg2rad :: a
    deg2rad = pi / 180
    -- Julian centuries from Jan 1 2000 12h epoch (ut1)
    tut1 :: Rational
    tut1 = (jdut1 - 2451545.0) / 36525.0
    temp :: Rational
    temp = (-6.2e-6) * tut1 * tut1 * tut1 + 0.093104 * tut1 * tut1
           + (876600.0 * 3600.0 + 8640184.812866) * tut1 + 67310.54841
    -- 360/86400 = 1/240, to deg, to rad
    temp' :: a
    temp' = mod' (fromRational temp * deg2rad / 240) (2 * pi)
    in GMST $ normalizeAngle $ Radians temp'

getCurrentUT1 :: DUT -> IO UniversalTime
getCurrentUT1 dut = utcToUT1 dut <$> getCurrentTime

getCurrentJulianDate :: DUT -> IO JulianDate
getCurrentJulianDate dut = julianDateFromUniversalTime <$> getCurrentUT1 dut

getCurrentGMST :: DUT -> IO (GMST Double)
getCurrentGMST dut = gmstFromJulianDate <$> getCurrentJulianDate dut

getCurrentTAI :: LeapSecondMap -> IO (Maybe AbsoluteTime)
getCurrentTAI lsm = utcToTAITime lsm <$> getCurrentTime

getCurrentTT :: LeapSecondMap -> IO (Maybe TT)
getCurrentTT lsm = fmap ttFromTAI <$> getCurrentTAI lsm
