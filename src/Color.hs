{-# LANGUAGE DeriveGeneric #-}

module Color
    ( Color (..)
    , (^*^)
    ) where

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Control.Applicative
import Linear

data Color f = RGB f f f deriving (Show, Eq, Generic, Generic1)

(^*^) :: (Num f) => Color f -> Color f -> Color f
(^*^) (RGB r0 g0 b0) (RGB r1 g1 b1) = RGB (r0 * r1) (g0 * g1) (b0 * b1)

instance Functor Color where
    fmap f (RGB r g b) = RGB (f r) (f g) (f b)
    a <$ _ = RGB a a a

instance Applicative Color where
    pure a = RGB a a a
    RGB r0 g0 b0 <*> RGB r1 g1 b1 = RGB (r0 r1) (g0 g1) (b0 b1)

instance Additive Color where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

instance NFData a => NFData (Color a)
instance NFData1 Color

