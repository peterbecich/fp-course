{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
-- import Data.Set
import qualified Data.Set as S

import Data.Bifunctor

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec =
  error "todo: Course.State#exec"

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval =
  error "todo: Course.State#eval"

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get =
  error "todo: Course.State#get"

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put =
  error "todo: Course.State#put"

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)

counter :: State Int Int
counter = State (\s -> (s+1, s+1))

rs :: Int -> (Int, Int)
rs = runState counter
one :: (Int, Int)
one = rs 0

foo :: Int -> Bool
foo i = x
  where x = i > 100

bar :: Int -> Bool
bar i = x > 100
  where x = i

run' :: State Int Int -> Int -> (Int, Int)
run' (State runState0) s0 = runState0 s0

instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
  (<$>) f (State runState0) = let
    runState1 s0 = bimap f id (runState0 s0)
    in State runState1

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (\s0 -> (x, s0))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) stateAB stateA = let
    runStateB s0 = let
      (ab, s1) = runState stateAB s0
      (a, s2) = runState stateA s1
      in (ab a, s2)
    in State runStateB



                                     
  -- (<*>) stateAB stateA = let
  --   -- runStateB :: s -> (b, s)
  --   runStateB s0 = let
  --     -- out1 :: (a -> b, s)
  --     out1 = runState stateAB s0
  --     -- out2 :: (a, s)
  --     out2 = runState stateA $ snd out2
  --     f = fst out1
  --     x = fst out2
  --     -- y :: b
  --     y = f x
  --     in (y, snd out2)
  --   in State runStateB

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) :: (a -> State s b) -> State s a -> State s b
  (=<<) stateF stateA = let
    runStateB s0 = let
      (x, s1) = runState stateA s0
      (y, s2) = runState (stateF x) s1
      in (y, s2)
    in State runStateB


-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a -> s -> s
exec stateSA s0 = snd $ runState stateSA s0

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval stateSA s0 = fst $ runState stateSA s0

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
get = State (\s0 -> (s0, s0))


-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()   -- 
put s0 = State (\_ -> ((), s0))

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM g (x:.xs) = let
  -- fBool :: f Bool
  fBool = g x
  -- h :: Bool -> f (Optional a)
  h b = if (b==True) then pure (Full x) else findM g xs
  in h =<< fBool
findM _ Nil = pure Empty


-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3

-- import qualified Data.Set as S
firstRepeat :: Ord a => List a -> Optional a
firstRepeat lx = fst $ runState (findM firstRepeatPredicate' lx) S.empty

firstRepeatPredicate :: Ord a => a -> State (S.Set a) Bool
firstRepeatPredicate x = do
  set <- get
  let exsts = S.member x set :: Bool
  put $ S.insert x set
  return exsts
      
firstRepeatPredicate' :: Ord a => a -> State (S.Set a) Bool
firstRepeatPredicate' x =
  get >>= (\set -> put (S.insert x set) >> return (S.member x set))

firstRepeat' :: Ord a => List a -> Optional a
firstRepeat' lx = firstRepeatHelper lx S.empty

firstRepeatHelper :: Ord a => List a -> (S.Set a) -> Optional a
firstRepeatHelper (x:.xs) sx
  | S.member x sx = Full x
  | otherwise = firstRepeatHelper xs $ S.insert x sx
firstRepeatHelper Nil _ = Empty

firstRepeat'' lx = firstRepeatHelper' lx S.empty

firstRepeatHelper' :: Ord a => List a -> (S.Set a) -> Optional a
firstRepeatHelper' lx sx = find (\x -> S.member x sx) lx

repeatsFoo = 1:.2:.1:.4:.Nil
repeatsInfinitely = repeatsFoo ++ repeatsInfinitely

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)

-- emptySet :: Ord a => State (S.Set a) ()
-- emptySet = put S.empty

distinct :: Ord a => List a -> List a
distinct la = fst $ runState (filtering ((not <$>) . firstRepeatPredicate) la) S.empty

-- distinctPredicate :: Ord a => a -> State (S.Set a) Bool
-- distinctPredicate x = do
--   set <- get
--   let exsts = S.member x set :: Bool
--   put $ S.insert x set
--   return exsts

distinct' :: Ord a => List a -> S.Set a
distinct' la = foldRight S.insert S.empty la

baz = (1 :. 1:. 2 :. 1:. 4 :. Nil)

distinctExample' = distinct' (1 :. 1:. 2 :. 1:. 4 :. Nil)

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy =
  error "todo: Course.State#isHappy"
