{-|
Module      : Linear.Geo.ECI.TEME
Copyright   : Travis Whitaker 2025
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

True Equator Mean Equilon (TEME) ECI coordinates.

-}

{-# LANGUAGE DataKinds
           , DeriveDataTypeable
           , DeriveGeneric
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , TypeFamilies
           #-}

module Linear.Geo.ECI.TEME where

import Control.DeepSeq (NFData)

import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)

import Data.Coerce

import Data.Data (Data)

import Data.Distributive

import Data.Fixed (mod')

import qualified Data.Vector as V

import GHC.Generics

import qualified Linear.Affine  as L
import qualified Linear.Epsilon as L
import qualified Linear.Matrix  as L
import qualified Linear.Metric  as L
import qualified Linear.V       as L
import qualified Linear.V2      as L
import qualified Linear.V3      as L
import qualified Linear.Vector  as L


import Linear.Geo.ECEF
import Linear.Geo.PlaneAngle
import Linear.Geo.Time

newtype TEME a = TEME (L.V3 a)
             deriving stock ( Eq
                            , Ord
                            , Show
                            , Generic
                            , Data
                            , Bounded
                            )
             deriving newtype ( Num
                              , Fractional
                              , Floating
                              , Functor
                              , Applicative
                              , Monad
                              , MonadFix
                              , MonadZip
                              , Foldable
                              , L.Additive
                              , L.Metric
                              , L.Trace
                              , L.Epsilon
                              , NFData
                              )

instance Traversable TEME where
    traverse f ecef = traverse f (coerce ecef)

instance Distributive TEME where
    distribute f = TEME $ L.V3 (fmap (\(TEME (L.V3 x _ _)) -> x) f)
                               (fmap (\(TEME (L.V3 _ y _)) -> y) f)
                               (fmap (\(TEME (L.V3 _ _ z)) -> z) f)

instance L.Finite TEME where
    type Size TEME = 3
    toV (TEME (L.V3 x y z)) = L.V (V.fromListN 3 [x, y, z])
    fromV (L.V v)           = TEME $ L.V3 (v V.! 0) (v V.! 1) (v V.! 2)

instance L.R1 TEME where
    _x f (TEME (L.V3 x y z)) = (\x' -> TEME (L.V3 x' y z)) <$> f x

instance L.R2 TEME where
    _y  f (TEME (L.V3 x y z)) = (\y' -> TEME (L.V3 x y' z)) <$> f y
    _xy f (TEME (L.V3 x y z)) = (\(L.V2 x' y') -> TEME (L.V3 x' y' z))
                            <$> f (L.V2 x y)

instance L.R3 TEME where
    _z   f (TEME (L.V3 x y z)) = (\z' -> TEME (L.V3 x y z')) <$> f z
    _xyz f (TEME v)            = TEME <$> f v

instance L.Affine TEME where
    type Diff TEME = L.V3
    (TEME x) .-. (TEME y) = x L..-. y
    (TEME x) .+^ y        = TEME (x L..+^ y)
    (TEME x) .-^ y        = TEME (x L..-^ y)

-- | Right-handed orthogonal vector with magnitude equal to the area of the
--   subtended parallelogram.
cross :: Num a => TEME a -> TEME a -> TEME a
cross x y = TEME $ L.cross (coerce x) (coerce y)

-- | Scalar triple product.
triple :: Num a => TEME a -> TEME a -> TEME a -> a
triple x y z = L.triple (coerce x) (coerce y) (coerce z)

polarm80 :: Floating a => Radians a -> Radians a -> L.M33 a
polarm80 (Radians xp) (Radians yp) =
    let cosxp = cos xp
        sinxp = sin xp
        cosyp = cos yp
        sinyp = sin yp
    in L.V3 (L.V3 cosxp 0.0 (-sinxp))
            (L.V3 (sinxp * sinyp) cosyp (cosxp * sinyp))
            (L.V3 (sinxp * cosyp) (-sinyp) (cosxp * cosyp))
      
-- | The conversion from TEME to ECEF changes over time with the position of the
--   earth. Specifically it depends on current time values derived from clocks
--   operating on the surface of the Earth, as well as sidereal time.
fromTEME :: forall a.
            ( Real a
            , Floating a
            )
         => TT
         -> JulianDate
         -> Radians a -- ^ Earth orientation parameter X
         -> Radians a -- ^ Earth orientation parameter Y
         -> TEME a
         -> ECEF a
fromTEME tt jd xp yp (TEME r) =
    let deg2rad = pi / 180.0;
        (GMST (Radians gmst)) = gmstFromJulianDate jd
        ttt :: a
        ttt = fromRational (toRational (ttSeconds tt) / (86400 * 365.25 * 100))
        -- find omega from nutation theory
        omega' = 125.04452222
            + ((-6962890.5390)*ttt + 7.455*ttt*ttt + 0.008*ttt*ttt*ttt) / 3600.0
        omega = mod' omega' 360.0 * deg2rad

        gmstg'
            | (coerce jd :: Rational) > 2450449.5 =
                gmst + 0.00264*pi / (3600*180) * sin omega
                    + 0.000063*pi / (3600*180)* sin (2.0 *omega)
            | otherwise = gmst

        gmstg = mod' gmstg' (2*pi)

        st = L.V3 (L.V3 (cos gmstg) (-(sin gmstg)) 0)
                  (L.V3 (sin gmstg) (cos gmstg)    0)
                  (L.V3 0           0              1)

        pm = polarm80 xp yp 

        rpef = L.transpose st L.!* r
        recef = L.transpose pm L.!* rpef
    in ECEF recef
