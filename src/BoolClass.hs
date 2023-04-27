module BoolClass where

import Prelude (Show (..), (<>), undefined) -- for show instances
import qualified Data.Bool as Bool

import MyPrelude

-- | The class of Boolean-like types (types having a notion of
-- 'true' and 'false' and 'bool'ean choice).
-- Instances should satisfy the following:
--
-- [Bool True] @'bool' f t 'true' = t@
-- [Bool False]  @'bool' f t 'false' = f@
class BoolClass b where
  false :: b
  true :: b
  bool :: a -> a -> b -> a

instance BoolClass Bool.Bool where
  true = Bool.True
  false = Bool.False
  bool = Bool.bool

-- | if-then-else
ite :: BoolClass b => b -> a -> a -> a
ite c t e = bool e t c

-- >>> ite (true :: Bool.Bool) 1 2
-- 1

-- | Boolean "and"
(&&) :: BoolClass b => b -> b -> b
(&&) t1 t2 = bool false t1 t2

-- >>> true && false :: Bool.Bool
-- False

-- | Boolean "or",
(||) :: BoolClass b => b -> b -> b
(||) t1 t2 = bool t1 true t2

-- >>> true || false :: Bool.Bool
-- True

-- | Boolean "not"
not :: BoolClass b => b -> b
not t = ite t false true

-- >>>  not true :: Bool.Bool
-- False

newtype CBool = CBool { getCBool :: forall a. a -> a -> a}

instance BoolClass CBool where
  true  = CBool (\f t -> t)
  false = CBool (\f t -> f)
  bool f t b = getCBool b f t

-- >>> ite (true :: CBool) 1 2
-- 1

-- | converting between different instances of 'BoolClass'
fromBoolClass :: (BoolClass a, BoolClass b) => a -> b
fromBoolClass = bool false true

-- | 'Show' instance for 'CBool' (via transformation into Haskell Bool)
instance Show CBool where
  show cb = "C" <> show (fromBoolClass cb :: Bool.Bool)

-- >>> true && false :: CBool
-- CFalse

-- >>> true || false :: CBool
-- CTrue

-- >>> not true :: CBool
-- CFalse