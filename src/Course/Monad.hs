{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative f => Monad f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15

(<**>) ::
  Monad f =>
  f (a -> b)
  -> f a
  -> f b
(<**>) =
  error "todo: Course.Monad#(<**>)"

(<*>) :: Monad f => f (a -> b) -> f a -> f b
(<*>) ff fx = error "todo: Course.Monad (=<<)#instance Id"


-- (=<<) :: Monad f => (a -> f b) -> f a -> f b
-- (pure) :: Applicative f => a -> f a
-- (<$>) :: Functor f => (a -> b) -> f a -> f b  
(<*>) :: Monad f => f (a -> b) -> f a -> f b
(<*>) fg fx = (\g -> g <$> fx ) =<< fg

infixl 4 <**>

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> Id b)
    -> Id a
    -> Id b
  (=<<) g (Id x) = g x

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) g (x:.xs) = (g x) ++ (g =<< xs)
  (=<<) _ Nil = Nil


-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) f (Full x) = f x
  (=<<) _ Empty = Empty

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) :: (a -> ((->) t b)) -> ((->) t a) -> ((->) t b)
  (=<<) afunctb functa t = let
    a = functa t
    functb = afunctb a
    in functb t


-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Monad f =>
  f (f a)
  -> f a
join ffx = id =<< ffx

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) :: Monad f => f a -> (a -> f b) -> f b
(>>=) fa afb = join $ afb <$> fa

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) :: Monad f => (b -> f c) -> (a -> f b) -> a -> f c
(<=<) bfc afb = (\fb -> fb >>= bfc) . afb

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
