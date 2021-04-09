{-# LANGUAGE DataKinds
           , DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           , ScopedTypeVariables
           , TypeFamilies
           #-}

module Linear.Geo.ENU (
    ENU(..)
  , alignOrigin
  , liftAO2
  , liftAO2V
  , enuToECEF
  , ecefToENU
  , disp
  , diff
  , lerp
  , dot
  , quadrance
  , norm
  , distance
  , normalize
  , project
  ) where

import Control.DeepSeq (NFData)

import Data.Data (Data)

import GHC.Generics (Generic)

import qualified Linear.Affine  as L
import qualified Linear.Epsilon as L
import qualified Linear.Matrix  as L
import qualified Linear.Metric  as L
import qualified Linear.V2      as L
import qualified Linear.V3      as L
import qualified Linear.Vector  as L

import Linear.Geo.ECEF
import Linear.Geo.Geodetic
import Linear.Geo.PlaneAngle

-- | R3 vector with the origin located at some arbitrary 'ECEF' position vector,
--   first basis pointing east at the origin, second basis vector pointing north
--   at the origin, and third basis vector normal to the plane tangent to the
--   ellipsoid at the origin.
--
--   Each value records both the ENU vector and the ENU origin. Most functions
--   of multiple ENU values will require the points to occupy coordinal frames.
--   Binary operations on ENU values should preserve the coordinate frame of the
--   /left/ value.
--
--   The 'Eq' and 'Ord' instances for this type implement structural equality,
--   i.e. ENU points with different 'enuOrigin' values will never be equal.
--   Floating point errors limit the usefulness of
--   exact-equality-as-coincidence.
--
--   Operations on ENU points use the uncorrected WGS84 geoid model.
data ENU a = ENU {
    enuOrigin :: ECEF a
  , enuPoint  :: L.V3 a
  } deriving ( Eq
             , Ord
             , Show
             , Generic
             , Data
             , Bounded
             , NFData
             )

instance L.R1 ENU where
    _x f (ENU o (L.V3 x y z)) = (\x' -> ENU o (L.V3 x' y z)) <$> f x

instance L.R2 ENU where
    _y  f (ENU o (L.V3 x y z)) = (\y' -> ENU o (L.V3 x y' z)) <$> f y
    _xy f (ENU o (L.V3 x y z)) = (\(L.V2 x' y') -> ENU o (L.V3 x' y' z))
                             <$> f (L.V2 x y)

instance L.R3 ENU where
    _z   f (ENU o (L.V3 x y z)) = (\z' -> ENU o (L.V3 x y z')) <$> f z
    _xyz f (ENU o v)            = ENU o <$> f v

-- | Align the second argument with the coordinate system of the first.
alignOrigin :: RealFloat a => ENU a -> ENU a -> ENU a
alignOrigin (ENU xo _) y@(ENU yo _)
    | xo == yo  = y
    | otherwise = ecefToENU xo (enuToECEF y)

-- | Lift a function on vectors to a function on origin-aligned ENU points.
liftAO2 :: RealFloat a => (L.V3 a -> L.V3 a -> b) -> ENU a -> ENU a -> b
liftAO2 f x@(ENU _ xp) y = let (ENU _ y'p) = alignOrigin x y
                           in f xp y'p

-- | Lift a binary operation on vectors to a binary operation on origin-aligned
--   ENU points.
liftAO2V :: RealFloat a
         => (L.V3 a -> L.V3 a -> L.V3 a)
         -> ENU a
         -> ENU a
         -> ENU a
liftAO2V f x@(ENU xo xp) y = let (ENU _ y'p) = alignOrigin x y
                             in ENU xo (f xp y'p)

enuToECEF :: RealFloat a => ENU a -> ECEF a
enuToECEF (ENU o x) =
    let (Geo (Radians po) (Radians lo) _) = ecefToGeo o
        rot           =
            L.V3 (L.V3 (-(sin lo)) ((-(cos lo)) * (sin po))  ((cos lo) * (cos po)))
                 (L.V3 (cos lo)    ((- (sin lo)) * (sin po)) ((sin lo) * (cos po)))
                 (L.V3 0           (cos po)                  (sin po)             )
    in o L..+^ (rot L.!* x)

ecefToENU :: RealFloat a
          => ECEF a -- ^ Origin
          -> ECEF a -- ^ Point
          -> ENU a
ecefToENU o@(ECEF vo) (ECEF vp) =
    let (Geo (Radians po) (Radians lo) _) = ecefToGeo o
        rot           =
            L.V3 (L.V3 (-(sin lo))              (cos lo)                 0       )
                 (L.V3 ((-(cos lo)) * (sin po)) ((-(sin lo)) * (sin po)) (cos po))
                 (L.V3 ((cos lo) * (cos po))    ((sin lo) * (cos po))    (sin po))
        x = rot L.!* (vp - vo)
    in ENU o x

-- | Affine addition. Apply a displacement vector.
disp :: Num a => ENU a -> L.V3 a -> ENU a
disp (ENU o p) v = (ENU o (p + v))

-- | Affine subtraction. Get the vector from the first to the second ENU point.
diff :: RealFloat a => ENU a -> ENU a -> L.V3 a
diff x y = enuPoint $ liftAO2V (L..-.) x y

-- | Linearly interpolate between two points.
lerp :: RealFloat a => a -> ENU a -> ENU a -> ENU a
lerp f = liftAO2V (L.lerp f)

dot :: RealFloat a => ENU a -> ENU a -> a
dot = liftAO2 L.dot

quadrance :: Num a => ENU a -> a
quadrance = L.quadrance . enuPoint

norm :: Floating a => ENU a -> a
norm = L.norm . enuPoint

distance :: RealFloat a => ENU a -> ENU a -> a
distance = liftAO2 L.distance

normalize :: (Floating a, L.Epsilon a) => ENU a -> ENU a
normalize (ENU xo xp) = ENU xo (L.normalize xp)

project :: RealFloat a => ENU a -> ENU a -> ENU a
project = liftAO2V L.project
