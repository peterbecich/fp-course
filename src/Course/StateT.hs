{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P
import Data.Bifunctor (bimap)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) :: (a -> b) -> StateT s f a -> StateT s f b
  (<$>) func stateTSFA = StateT (\s0 -> let
                                    fas = runStateT stateTSFA s0 -- :: f (a, s)
                                    in (bimap func id) <$> fas
                                )
    

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure :: a -> StateT s f a
  pure x = StateT (\s0 -> pure (x, s0))

  (<*>) :: StateT s f (a -> b) -> StateT s f a -> StateT s f b
  (<*>) stateTSFAB stateTSFA = StateT (\s0 -> let
                                         fabs = runStateT stateTSFAB s0 -- :: f (a -> b, s)
                                         in fabs >>= (\tup -> let
                                                      ab = fst tup -- :: a -> b
                                                      s1 = snd tup -- :: s
                                                      fas = runStateT stateTSFA s1 -- :: f (a, s)
                                                      fbs = (bimap ab id) <$> fas -- :: f (b, s)
                                                      in fbs
                                                  ) -- :: f (b, s)
                                      )
                                                      
                                                      


-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  --(=<<) :: (a -> StateT s f b) -> StateT s f a -> StateT s f b
  (=<<) aStateTSFB stateTSFA = let
    sfa = runStateT stateTSFA -- :: s -> f (a, s)
    transition s0 = let
      fa = sfa s0 -- :: f (a, s)
      fStateTSFB = fmap (\tup -> (aStateTSFB (fst tup), snd tup)) fa -- :: f (StateT s f b, s)
      ffsbs = fmap (\tup -> (runStateT (fst tup) (snd tup), snd tup)) fStateTSFB -- :: f ( f (b, s), s )
      ffsb = fmap (\tup -> fst tup) ffsbs
      fsb = join ffsb -- :: f ( b, s )
      in fsb
    in StateT transition

  -- (=<<) aStateTSFB stateTSFA = let
  --   -- func' :: s -> f ( b, s )
  --   func' s0 = let 
  --        fas = runStateT stateTSFA s0 -- :: f (a, s)
  --        -- func :: a -> f (b, s)
  --        func x = let
  --          stateTSFB = aStateTSFB x -- :: StateT s f b
  --          fStateTSFB = pure stateTSFB -- :: f (StateT s f b)
  --          ffsb = runStateT <$> fStateTSFB -- :: f ( f ( b, s ) )
  --          in (join ffsb)
         
  --        fbs = fas >>= func
  --        in fbs -- :: f ( b, s )
  --   in StateT func'

                               
-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' :: (s -> (a, s)) -> State' s a
state' sas = StateT $ Id . sas

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' stateSA s0 = let
  idAS = runStateT stateSA s0
  in runId idAS

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT :: Functor f => StateT s f a -> s -> f s
execT stateTSFA s0 = let
  fas = runStateT stateTSFA s0 -- :: f (a, s)
  in (\tup -> snd tup) <$> fas


-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: State' s a -> s -> s
exec' stateSA s0 = let
  idS = execT stateSA s0 -- :: Id s
  in runId idS
  

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: Functor f => StateT s f a -> s -> f a
evalT stateTSFA s0 = let
  fas = runStateT stateTSFA s0 -- :: f (a, s)
  in (\tup -> fst tup) <$> fas


-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' :: State' s a -> s -> a
eval' stateSA s0 = let
  idA = evalT stateSA s0 -- :: Id a
  in runId idA

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: Monad f => StateT s f s
getT = StateT (\s0 -> pure (s0, s0))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: Monad f => s -> StateT s f ()
putT s0 = StateT (\_ -> pure ((), s0))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: (Ord a, Num a) => List a -> List a
distinct' lx = let
  distinctFilter = filtering distinctPredicate'
  stateDistinct = distinctFilter lx
  in runId $ evalT stateDistinct S.empty

distinctPredicate' :: (Ord a, Num a) => a -> State' (S.Set a) Bool
distinctPredicate' x = do
  set <- getT
  let exsts = S.member x set
  _ <- putT $ S.insert x set
  return $ not exsts

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: (Ord a, Num a) => List a -> Optional (List a)
distinctF =
  error "todo: Course.StateT#distinctF"

distinctFPredicate :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
distinctFPredicate x = error "todo"
  -- do
  -- set <- getT
  -- let exsts = S.member x set
  --     op = if x > 100 then Empty else Full (not exsts)  
  -- _ <- putT $ S.insert x set
  -- return op
  
-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) func optionalTFA = OptionalT $ (\fOpa -> func <$> fOpa) <$> (runOptionalT optionalTFA)

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Applicative f => Applicative (OptionalT f) where
  pure x = OptionalT $ pure $ Full x
  -- (<*>) OptionalT f (Optional a -> Optional b) -> OptionalT f Optional a -> OptionalT f Optional b
  (<*>) = error "todo"
  -- (<*>) optionalTFOpAOpB optionalTFOpA = let
  --   fOpAOpB = runOptionalT optionalTFOpAOpB -- :: f (Optional a -> Optional b)
  --   fOpA = runOptionalT optionalTFOpA -- :: f (Optional a)
  --   fOpB = fOpAOpB <*> fOpA
  --   in OptionalT fOpB
  --((runOptionalT optionalTFAB) <*> (runOptionalT optionalTFA))


-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) aOptionalTfOpB optionalTFOpA = error "todo" -- let
    -- fOpA = runOptionalT optionalTFOpA -- :: f (Optional a)
    -- in fOpA >>= (\opA -> opA >>= (\a -> let
    --                 opOptionalTfOpB = aOptionalTfOpB a -- :: Optional (OptionalT (f (Optional b)))
                    

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) =
    error "todo: Course.StateT (<$>)#instance (Logger l)"

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure =
    error "todo: Course.StateT pure#instance (Logger l)"
  (<*>) =
    error "todo: Course.StateT (<*>)#instance (Logger l)"

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) =
    error "todo: Course.StateT (=<<)#instance (Logger l)"

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 =
  error "todo: Course.StateT#log1"

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG =
  error "todo: Course.StateT#distinctG"
