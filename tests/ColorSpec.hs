{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ColorSpec (spec) where

import Color
import Linear
import Test.Hspec
import Test.QuickCheck

inRange :: Float -> Bool
inRange x = 0 <= x && x <= 1

clamp0To1 :: Gen Float
clamp0To1 = arbitrary `suchThat` inRange

instance Arbitrary (RGB cs) where
    arbitrary = RGB <$> (V3 <$> clamp0To1 <*> clamp0To1 <*> clamp0To1)

spec :: Spec
spec = do
    describe "linear -> linear" $ do
        it "is identity" $
            property $ \(c :: RGB Linear) ->
                c == (convert c :: RGB Linear)
    describe "linear and sRGB" $ do
        it "are isomorphic" $
            property $ \(c :: RGB Linear) ->
                nearZero $ c - (convert (convert c :: RGB SRGB) :: RGB Linear)
