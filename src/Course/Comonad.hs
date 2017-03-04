{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Comonad where

import Course.Core
import Course.ExactlyOne
import Course.Extend

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class Extend f => Comonad f where
  copure :: f a -> a

-- | Implement the @Comonad@ instance for @ExactlyOne@.
--
-- >>> copure (ExactlyOne 7)
-- 7

instance Comonad ExactlyOne where
  copure ::
    ExactlyOne a
    -> a
  copure =
    error "todo: Course.Comonad copure#instance ExactlyOne"

instance Comonad Id where
  copure :: Id a -> a
  copure (Id x) = x


-- | Witness that all things with (<<=) and copure also have (<$>).
--

-- >>> (+10) <$$> ExactlyOne 7
-- ExactlyOne 17
(<$$>) ::
  Comonad f =>
  (a -> b)
  -> f a
  -> f b
(<$$>) =
  error "todo: Course.Comonad#(<$>)"

-- >>> (+10) <$> Id 7
-- Id 17
(<$>) :: Comonad f => (a -> b) -> f a -> f b
(<$>) func fx =  let
  -- ffunc :: f a -> b
  ffunc = func . copure
  in ffunc <<= fx


