{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hedgehog
import Hedgehog.Main
import qualified Hedgehog.Gen   as HG
import qualified Hedgehog.Range as HR

import qualified Linear as L
import Linear.Geo

-- from the numeric-limits package:

{-# SPECIALIZE maxValue :: Double #-}
{-# SPECIALIZE maxValue :: Float #-}
maxValue :: (RealFloat a) => a
maxValue = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b^n - 1) (u - n)

-- | The minimum (positive) normalized value for the type.
{-# SPECIALIZE minValue :: Double #-}
{-# SPECIALIZE minValue :: Float #-}
minValue :: (RealFloat a) => a
minValue = x
  where n = floatDigits x
        b = floatRadix x
        (l, _) = floatRange x
        x = encodeFloat (b^n - 1) (l - n - 1)

hugeValFRange :: Range Double
hugeValFRange = HR.exponentialFloatFrom 0 minValue maxValue

hugeValFs :: MonadGen m => m Double
hugeValFs = HG.double hugeValFRange

class ApproxEq a where
    (~=~) :: a -> a -> Bool

instance ApproxEq Float where
    x ~=~ y = (x - y) <= 0.0000001

instance ApproxEq Double where
    x ~=~ y = (x - y) <= 0.0000001

instance ApproxEq a => ApproxEq (Radians a) where
    (Radians x) ~=~ (Radians y) = x ~=~ y

instance ApproxEq a => ApproxEq (Degrees a) where
    (Degrees x) ~=~ (Degrees y) = x ~=~ y

instance ApproxEq a => ApproxEq (DMS a) where
    (DMS xd xm xs) ~=~ (DMS yd ym ys) = (xd ~=~ yd)
                                     && (xm ~=~ ym)
                                     && (xs ~=~ ys)

instance ApproxEq a => ApproxEq (DM a) where
    (DM xd xm) ~=~ (DM yd ym) = (xd ~=~ yd)
                             && (xm ~=~ ym)

instance ApproxEq a => ApproxEq (L.V3 a) where
    (L.V3 ax ay az) ~=~ (L.V3 bx by bz) = (ax ~=~ bx)
                                       && (ay ~=~ by)
                                       && (az ~=~ bz)

instance ApproxEq a => ApproxEq (ECEF a) where
    (ECEF x) ~=~ (ECEF y) = x ~=~ y

instance ApproxEq a => ApproxEq (Geo a) where
    (Geo ap al ah) ~=~ (Geo bp bl bh) = (ap ~=~ bp)
                                     && (al ~=~ bl)
                                     && (ah ~=~ bh)

instance (RealFloat a, ApproxEq a) => ApproxEq (ENU a) where
    x@(ENU _ xp) ~=~ y = let ENU _ y'p = alignOrigin x y
                         in xp ~=~ y'p

genECEF :: MonadGen m => m (ECEF Double)
genECEF = ECEF <$> (L.V3 <$> hugeValFs
                         <*> hugeValFs
                         <*> hugeValFs)

genENU :: MonadGen m => m (ENU Double)
genENU = do
    og <- genGeo
    pv <- L.V3 <$> hugeValFs
               <*> hugeValFs
               <*> hugeValFs
    pure (ENU (geoToECEF og) pv)

genGeo :: MonadGen m => m (Geo Double)
genGeo = Geo <$> (normalizeAngle <$> genRad)
             <*> (normalizeAngle <$> genRad)
             <*> hugeValFs

genRad :: MonadGen m => m (Radians Double)
genRad = Radians <$> hugeValFs

genDeg :: MonadGen m => m (Degrees Double)
genDeg = Degrees <$> hugeValFs

genDMS :: MonadGen m => m (DMS Double)
genDMS = DMS <$> hugeValFs
             <*> hugeValFs
             <*> hugeValFs

genDM :: MonadGen m => m (DM Double)
genDM = DM <$> hugeValFs
           <*> hugeValFs

radNormRange :: Property
radNormRange = property $ do
    r <- forAll genRad
    let (Radians r') = normalizeAngle r
    assert ((r' >= 0) && (r' < (2 * pi)))

radNormIdemp :: Property
radNormIdemp = property $ do
    r <- forAll genRad
    let r' = normalizeAngle r
    r' === normalizeAngle r'

radToRadFromRadIdemp :: Property
radToRadFromRadIdemp = property $ do
    r <- forAll genRad
    r === fromRadians (toRadians r)

degNormRange :: Property
degNormRange = property $ do
    d <- forAll genDeg
    let (Degrees d') = normalizeAngle d
    assert ((d' >= 0) && (d' < 360))

degNormIdemp :: Property
degNormIdemp = property $ do
    d <- forAll genDeg
    let d' = normalizeAngle d
    d' === normalizeAngle d'

degToRadFromRadIdemp :: Property
degToRadFromRadIdemp = property $ do
    d <- forAll genDeg
    let r = toRadians d
    d === fromRadians r

dmsNormRange :: Property
dmsNormRange = property $ do
    d <- forAll genDMS
    let dmsn = normalizeAngle d
        (Degrees d') = dmsToDegrees dmsn
    assert ((d' >= 0) && (d' < 360))

dmsNormIdemp :: Property
dmsNormIdemp = property $ do
    d <- forAll genDMS
    let d' = normalizeAngle d
    d' === normalizeAngle d'

dmsToRadFromRadIdemp :: Property
dmsToRadFromRadIdemp = property $ do
    d <- forAll genDMS
    let r = toRadians d
    d === fromRadians r

dmNormRange :: Property
dmNormRange = property $ do
    d <- forAll genDM
    let dmn = normalizeAngle d
        (Degrees d') = dmToDegrees dmn
    assert ((d' >= 0) && (d' < 360))

dmNormIdemp :: Property
dmNormIdemp = property $ do
    d <- forAll genDM
    let d' = normalizeAngle d
    d' === normalizeAngle d'

dmToRadFromRadIdemp :: Property
dmToRadFromRadIdemp = property $ do
    d <- forAll genDM
    let r = toRadians d
    d === fromRadians r

radToFromLatLonIdemp :: Property
radToFromLatLonIdemp = property $ do
    g <- forAll genGeo
    let p :: Radians Double
        l :: Radians Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    g === g'

degToFromLatLonIdemp :: Property
degToFromLatLonIdemp = property $ do
    g <- forAll genGeo
    let p :: Degrees Double
        l :: Degrees Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    g === g'

dmsToFromLatLonIdemp :: Property
dmsToFromLatLonIdemp = property $ do
    g <- forAll genGeo
    let p :: DMS Double
        l :: DMS Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    g === g'

dmToFromLatLonIdemp :: Property
dmToFromLatLonIdemp = property $ do
    g <- forAll genGeo
    let p :: DM Double
        l :: DM Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    g === g'

geoToFromECEFIdemp :: Property
geoToFromECEFIdemp = property $ do
    g <- forAll genGeo
    geoToECEF g === geoToECEF (ecefToGeo (geoToECEF g))

enuToFromECEFIdemp :: Property
enuToFromECEFIdemp = property $ do
    p@(ENU o _) <- forAll genENU
    p === ecefToENU o (enuToECEF p)

main :: IO ()
main = defaultMain $ (:[]) $ checkParallel $ Group "Linear.Geo"
    [ ("normalizeAngle range check @Radians", radNormRange)
    , ("normalizeAngle idempotent @Radians", radNormIdemp)
    , ("(fromRadians . toRadians) == id @Radians", radToRadFromRadIdemp)
    , ("normalizeAngle range check @Degrees", degNormRange)
    , ("normalizeAngle idempotent @Degrees", degNormIdemp)
    , ("(fromRadians . toRadians) == id @Degrees", degToRadFromRadIdemp)
    , ("normalizeAngle range check @DMS", dmsNormRange)
    , ("normalizeAngle idempotent @DMS", dmsNormIdemp)
    , ("(fromRadians . toRadians) == id @DMS", dmsToRadFromRadIdemp)
    , ("normalizeAngle range check @DM", dmNormRange)
    , ("normalizeAngle idempotent @DM", dmNormIdemp)
    , ("(fromRadians . toRadians) == id @DM", dmToRadFromRadIdemp)
    , ("(fromLatLon . toLatLon) == id @Radians", radToFromLatLonIdemp)
    , ("(fromLatLon . toLatLon) == id @Degrees", degToFromLatLonIdemp)
    , ("(fromLatLon . toLatLon) == id @DMS", dmsToFromLatLonIdemp)
    , ("(fromLatLon . toLatLon) == id @DM", dmToFromLatLonIdemp)
    , ("(ecefToGeo . geoToECEF) == id", geoToFromECEFIdemp)
    , ("(ecefToENU . enuToECEF) == id", enuToFromECEFIdemp)
    ]
