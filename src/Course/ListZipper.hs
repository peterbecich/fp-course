{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ListZipper where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Extend
import Course.Comonad
import Course.Traversable
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper (List a) a (List a)
  deriving Eq

lefts ::
  ListZipper a
  -> List a
lefts (ListZipper l _ _) =
  l

rights ::
  ListZipper a
  -> List a
rights (ListZipper _ _ r) =
  r

zipperLength :: ListZipper a -> Int
zipperLength lza = length (lefts lza) + 1 + length (rights lza)

focusPosition :: ListZipper a -> Int
focusPosition lza = length (lefts lza)

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

bindMaybeListZipper :: MaybeListZipper a -> (ListZipper a -> MaybeListZipper b) -> MaybeListZipper b
bindMaybeListZipper IsNotZ _ = IsNotZ
bindMaybeListZipper (IsZ lza) f = f lza

foldMaybeListZipper :: MaybeListZipper a -> a -> ListZipper a
foldMaybeListZipper (IsZ lza) _ = lza
foldMaybeListZipper IsNotZ focus = ListZipper Nil focus Nil

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  (<$>) func (ListZipper left x right) =
    ListZipper (func <$> left) (func x) (func <$> right)
    

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7]))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  (<$>) func (IsZ zpr) = IsZ $ func <$> zpr
  (<$>) _ IsNotZ = IsNotZ
    

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> xs == toListZ (fromList xs)
fromList :: List a -> MaybeListZipper a
fromList (x:.xs) = IsZ $ ListZipper Nil x xs
fromList Nil = IsNotZ

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> toOptional (fromOptional z) == z
toOptional :: MaybeListZipper a -> Optional (ListZipper a)
toOptional (IsZ zpr) = Full zpr
toOptional IsNotZ = Empty


zipper ::
  [a]
  -> a
  -> [a]
  -> ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOp ::
  Optional (ListZipper a)
  -> MaybeListZipper a
fromOp Empty =
  IsNotZ
fromOp (Full z) =
  IsZ z

asZipper ::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asZipper f =
  asMaybeZipper (IsZ . f)

(>$>)::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asMaybeZipper _ IsNotZ =
  IsNotZ
asMaybeZipper f (IsZ z) =
  f z

(-<<) ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(-<<) =
  asMaybeZipper

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList :: ListZipper a -> List a
toList (ListZipper left x right) = reverse left ++ ( x :. Nil ) ++ right

-- | Convert the given (maybe) zipper back to a list.
toListZ :: MaybeListZipper a -> List a
toListZ IsNotZ = Nil
toListZ (IsZ z) = toList z

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus :: (a -> a) -> ListZipper a -> ListZipper a
withFocus func (ListZipper left x right) = ListZipper left (func x) right


-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus :: a -> ListZipper a -> ListZipper a
setFocus x zpr = withFocus (\_ -> x) zpr


-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) :: ListZipper a -> a -> ListZipper a
(.=) = flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft :: ListZipper a -> Bool
hasLeft (ListZipper (_:._) _ _) = True
hasLeft (ListZipper Nil _ _) = False

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight :: ListZipper a -> Bool
hasRight (ListZipper _ _ (_:._)) = True
hasRight (ListZipper _ _ Nil) = False


-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> findLeft (const p) -<< fromList xs == IsNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]


-- :t break 
-- break :: (a -> Bool) -> List a -> (List a, List a)

-- fooz = zipper [1, 2, 1] 3 [4, 5]
-- foozl = lefts fooz

findLeft :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findLeft func (ListZipper left x right)
  | exists func left = let
      (left', right') = break func left
      in case right' of
        (r:.rs) -> IsZ $ ListZipper rs r (left'++(single x)++right)
        Nil -> IsNotZ
  | otherwise = IsNotZ


-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> findRight (const False) -<< fromList xs == IsNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
findRight :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findRight func (ListZipper left x right)
  | exists func right = let
      (left', right') = break func right
      in case right' of
           (r:.rs) -> IsZ $ ListZipper ((reverse left')++(single x)++left) r rs
  | otherwise = IsNotZ

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop ::
  ListZipper a
  -> ListZipper a
moveLeftLoop (ListZipper Nil x right) = let
  right' = reverse $ x :. right
  in ListZipper (tailOr Nil right') (headOr x right') Nil
moveLeftLoop (ListZipper (y:.ys) x right) = ListZipper ys y (x:.right)

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop :: ListZipper a -> ListZipper a
moveRightLoop (ListZipper left x Nil) = let
  left' = reverse $ x :. left
  in ListZipper Nil (headOr x left') (tailOr Nil left')
moveRightLoop (ListZipper left x (y:.ys)) = ListZipper (x:.left) y ys

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft :: ListZipper a -> MaybeListZipper a
moveLeft (ListZipper (z:.zs) x right) = IsZ $ ListZipper zs z (x:.right)
moveLeft (ListZipper Nil _ _) = IsNotZ

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight :: ListZipper a -> MaybeListZipper a
moveRight (ListZipper left x (z:.zs)) = IsZ $ ListZipper (x:.left) z zs
moveRight (ListZipper _ _ Nil) = IsNotZ

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft :: ListZipper a -> MaybeListZipper a
swapLeft (ListZipper (l:.ls) x right) =
  IsZ $ ListZipper (x:.ls) l right
swapLeft _ = IsNotZ

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight :: ListZipper a -> MaybeListZipper a
swapRight (ListZipper left x (r:.rs)) =
  IsZ $ ListZipper left r (x:.rs)
swapRight _ = IsNotZ


-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> dropLefts (zipper l x r) == zipper [] x r
dropLefts :: ListZipper a -> ListZipper a
dropLefts (ListZipper _ x right) = ListZipper Nil x right

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> dropRights (zipper l x r) == zipper l x []
dropRights :: ListZipper a -> ListZipper a
dropRights (ListZipper left x _) = ListZipper left x Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN :: Int -> ListZipper a -> MaybeListZipper a
moveLeftN n lz@(ListZipper _ _ _)
  | n == 0 = IsZ $ lz
  | n < 0 = moveRightN (-1*n) lz
  | otherwise = foldRight (\_ mlz -> bindMaybeListZipper mlz moveLeft) (IsZ lz) (listh [1..n])

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
--
-- >>> moveRightN 4 (zipper [3,2,1] 4 [5,6,7])
-- ><

moveRightN :: Int -> ListZipper a -> MaybeListZipper a
moveRightN n lz@(ListZipper _ _ _)
  | n == 0 = IsZ $ lz
  | n < 0 = moveLeftN (-1*n) lz
  | otherwise = foldRight (\_ mlz -> bindMaybeListZipper mlz moveRight) (IsZ lz) (listh [1..n])

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
moveLeftN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveLeftN' n lza@(ListZipper left _ _)
  | n >= 0 = let
      mlza = moveLeftN n lza
    in case mlza of
         (IsZ lza') -> Right lza'
         IsNotZ -> Left $ length left
  | otherwise = moveRightN' (-1*n) lza

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 4 (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveRightN' n lza@(ListZipper _ _ right)
  | n >= 0 = let
      mlza = moveRightN n lza
    in case mlza of
         (IsZ lza') -> Right lza'
         IsNotZ -> Left $ length right
  | otherwise = moveLeftN' (-1*n) lza

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth :: Int -> ListZipper a -> MaybeListZipper a
nth n lza -- use where to avoid recalculating focus position
  | n < 0 || n > (zipperLength lza) = IsNotZ
  | n == (focusPosition lza) = IsZ lza
  | n < (focusPosition lza) = moveLeftN ((focusPosition lza) - n) lza
  | otherwise = moveRightN (n - (focusPosition lza)) lza

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> optional True (\z' -> index z' == i) (toOptional (nth i z))
index :: ListZipper a -> Int
index = focusPosition


-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> toList lz == toList (end lz)
--
-- prop> rights (end lz) == Nil
end :: ListZipper a -> ListZipper a
end lza@(ListZipper _ x _) = let
  lastPosition = zipperLength lza - 1
  in foldMaybeListZipper (nth lastPosition lza) x


-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> toList lz == toList (start lz)
--
-- prop> lefts (start lz) == Nil
start :: ListZipper a -> ListZipper a
start lza@(ListZipper _ x _) = foldMaybeListZipper (nth 0 lza) x

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft :: ListZipper a -> MaybeListZipper a
deletePullLeft (ListZipper (l:.ls) _ right) = IsZ $ ListZipper ls l right
deletePullLeft (ListZipper Nil _ _) = IsNotZ

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight :: ListZipper a -> MaybeListZipper a
deletePullRight (ListZipper left _ (r:.rs)) = IsZ $ ListZipper left r rs
deletePullRight (ListZipper _ _ Nil) = IsNotZ

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft :: a -> ListZipper a -> ListZipper a
insertPushLeft y (ListZipper left x right) = ListZipper (x:.left) y right

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight :: a -> ListZipper a -> ListZipper a
insertPushRight y (ListZipper left x right) = ListZipper left y (x:.right)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> all . (==) <*> take n . lefts . pure
--
-- prop> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]

instance Applicative ListZipper where
-- /Tip:/ Use @List#repeat@.
  pure x = ListZipper (repeat x) x (repeat x)
-- /Tip:/ Use `zipWith`
  (<*>) (ListZipper leftf f rightf) (ListZipper leftx x rightx) =
    ListZipper (zipWith (\g y -> g y) leftf leftx) (f x) (zipWith (\g y -> g y) rightf rightx)

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> let is (IsZ z) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> let is (IsZ z) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> IsNotZ <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsNotZ
-- ><
--
-- >>> IsNotZ <*> IsNotZ
-- ><
instance Applicative MaybeListZipper where
  pure x = IsZ $ pure x
  (<*>) (IsZ lzf) (IsZ lzx) = IsZ $ lzf <*> lzx
  (<*>) _ _ = IsNotZ

-- TODO

-- unfoldrZipper :: (a -> MaybeListZipper 

-- -- | Implement the `Extend` instance for `ListZipper`.
-- -- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
-- --
-- -- /Tip:/ Use @List#unfoldr@.
-- --
-- -- >>> id <<= (zipper [2,1] 3 [4,5])
-- -- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
-- instance Extend ListZipper where
--   -- (<<=) :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
--   (<<=) = undefined
--   -- (<<=) func lzx@(ListZipper left x right) = let
    


-- -- | Implement the `Extend` instance for `MaybeListZipper`.
-- -- This instance will use the `Extend` instance for `ListZipper`.
-- --
-- --
-- -- id <<= IsNotZ
-- -- ><
-- --
-- -- >>> id <<= (IsZ (zipper [2,1] 3 [4,5]))
-- -- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
-- instance Extend MaybeListZipper where
--   (<<=) =
--     error "todo: Course.ListZipper (<<=)#instance MaybeListZipper"

-- -- | Implement the `Comonad` instance for `ListZipper`.
-- -- This implementation returns the current focus of the zipper.
-- --
-- -- >>> copure (zipper [2,1] 3 [4,5])
-- -- 3
-- instance Comonad ListZipper where
--   copure (ListZipper _ x _) = x

-- -- | Implement the `Traversable` instance for `ListZipper`.
-- -- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- -- An effectful zipper is returned.
-- --
-- -- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- -- Full [1,2,3] >4< [5,6,7]
-- --
-- -- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- -- Empty
-- instance Traversable ListZipper where
--   traverse =
--     error "todo: Course.ListZipper traverse#instance ListZipper"

-- -- | Implement the `Traversable` instance for `MaybeListZipper`.
-- --
-- -- /Tip:/ Use `traverse` for `ListZipper`.
-- --
-- -- >>> traverse id IsNotZ
-- -- ><
-- --
-- -- >>> traverse id (IsZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- -- Full [1,2,3] >4< [5,6,7]
-- instance Traversable MaybeListZipper where
--   traverse =
--     error "todo: Course.ListZipper traverse#instance MaybeListZipper"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "><"
