{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  -- (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  (<$>) func ( Compose fga ) = Compose $ (\ga -> (func <$> ga)) <$> fga


instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure x = let
    -- gx :: g x
    gx = pure x
    -- fgx :: f ( g x )
    fgx = pure gx
    in Compose fgx

-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) (Compose fgab) (Compose fga) =
    Compose $ lift2 (\gab ga -> lift2 (\ab a -> ab a) gab ga) fgab fga


-- monads in general do not compose
-- they always compose with an instance of Traversable
-- requires sequence method of Traversable
instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) aCfgb (Compose fga) = error "needs Traversable"
    
