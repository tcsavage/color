{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Color where

import Control.Lens.Lens (lens, Lens')
import Data.Fixed (mod')
import Data.Proxy
import Foreign.Storable
import Linear

class ColorSpace cs where
    fromLinear :: Proxy cs -> Float -> Float
    toLinear :: Proxy cs -> Float -> Float

newtype Converter a b = Converter { converterFun :: Float -> Float }

converter :: forall a b. (ColorSpace a, ColorSpace b) => Converter a b
converter = Converter (fromLinear (Proxy :: Proxy b) . toLinear (Proxy :: Proxy a))

data Linear

instance ColorSpace Linear where
    fromLinear = const id
    toLinear = const id

data SRGB

instance ColorSpace SRGB where
    fromLinear _ v
        | v <= 0.0031308 = v * 12.92
        | otherwise = (1 + a) * (v ** (1 / 2.4)) - a
        where
            a = 0.055
    toLinear _ v
        | v <= 0.0031308 = v * (1 / 12.92)
        | otherwise = ((v + a) * (1 / (1 + a))) ** 2.4
        where
            a = 0.055

class Color a where
    toRGB :: a cs -> RGB cs
    applyConverter :: (ColorSpace cs0, ColorSpace cs1) => Converter cs0 cs1 -> a cs0 -> a cs1 

convert :: (Color a, ColorSpace csa, ColorSpace csb) => a csa -> a csb
convert = applyConverter converter

newtype RGB cs = RGB { rgbColor :: V3 Float } deriving (Eq, Show, Read, Storable, Num, Fractional, Epsilon)

linearRGB :: Float -> Float -> Float -> RGB Linear
linearRGB r g b = RGB $ V3 r g b

sRGB :: Float -> Float -> Float -> RGB SRGB
sRGB r g b = RGB $ V3 r g b

_rgbVec :: Lens' (RGB cs) (V3 Float)
_rgbVec = lens rgbColor (\_ vec -> RGB vec)

_r :: Lens' (RGB cs) Float
_r = _rgbVec . _x

_g :: Lens' (RGB cs) Float
_g = _rgbVec . _y

_b :: Lens' (RGB cs) Float
_b = _rgbVec . _z

instance Color RGB where
    toRGB = id
    applyConverter (Converter f) c = c { rgbColor = f <$> rgbColor c }

newtype HSV cs = HSV { hsvColor :: V3 Float } deriving (Eq, Show, Read, Storable, Num, Fractional, Epsilon)

linearHSV :: Float -> Float -> Float -> HSV Linear
linearHSV h s v = HSV $ V3 (h `mod'` 360) s v

sRGBHSV :: Float -> Float -> Float -> HSV SRGB
sRGBHSV h s v = HSV $ V3 (h `mod'` 360) s v

_hsvVec :: Lens' (HSV cs) (V3 Float)
_hsvVec = lens hsvColor (\_ vec -> HSV vec)

_h :: Lens' (HSV cs) Float
_h = _hsvVec . _x

_s :: Lens' (HSV cs) Float
_s = _hsvVec . _y

_v :: Lens' (HSV cs) Float
_v = _hsvVec . _z

instance Color HSV where
    toRGB (HSV (V3 h s v)) = RGB $ fmap (+ m) base
        where
            c = v * s
            m = v - c
            h' = h / 60
            x = c * (1 - abs ((h' `mod'` 2) - 1))
            base | 0 <= h' && h' < 1 = V3 c x 0
                 | 1 <= h' && h' < 2 = V3 x c 0
                 | 2 <= h' && h' < 3 = V3 0 c x
                 | 3 <= h' && h' < 4 = V3 0 x c
                 | 4 <= h' && h' < 5 = V3 x 0 c
                 | 5 <= h' && h' < 6 = V3 c 0 x
                 | otherwise = 0
    applyConverter (Converter f) c = c { hsvColor = V3 id id f <*> hsvColor c }