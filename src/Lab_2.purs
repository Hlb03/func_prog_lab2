module Lab2 where

import Prelude

import Data.List (List(..), length, reverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex fn = go 0
  where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  go n (Cons x xs)| fn x = Just n
                  | otherwise = go (n + 1) xs

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex fn xs = go (length(xs) - 1) (reverse(xs))
  where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  go n (Cons a as)| fn a = Just n
                  | otherwise = go (n - 1) as       

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip xs ys = reverse $ go xs ys Nil
  where
  go Nil _ acc = acc
  go _ Nil acc = acc
  go (Cons a as) (Cons b bs) acc = go as bs $ Cons (Tuple a b) acc

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip xy = go xy Nil Nil
  where
  go Nil acc1 acc2 = Tuple (reverse acc1) (reverse acc2)
  go (Cons a as) acc1 acc2 = go as (Cons (fst a) acc1) (Cons (snd a) acc2)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (Cons x xs)| p x = Cons x $ filter p xs
                    | otherwise = filter p xs

tailRecursionFilter :: forall a. (a -> Boolean) -> List a -> List a
tailRecursionFilter p = go Nil
  where
  go acc Nil = reverse acc
  go acc (Cons x xs)
    | p x = go (Cons x acc) xs
    | otherwise = go acc xs

take :: forall a. Int -> List a -> List a
take n _ | n < 1 = Nil
take _ Nil = Nil
take n (Cons x xs) = Cons x $ take (n-1) xs

tailRecursionTake :: forall a. Int -> List a -> List a
tailRecursionTake = go Nil
  where
  go acc n _ | n < 1 = reverse acc
  go acc _ Nil = reverse acc
  go acc n (Cons x xs) = go (Cons x acc) (n - 1) xs


test :: Effect Unit
test = do
  -- lists decalration to represent tests
  let firstList = Cons 5 (Cons 12 (Cons 3 (Cons 12 Nil)))
  let secondList = Cons "first" (Cons "second" (Cons "third" (Cons "fourth" Nil)))
  let combinedList = zip firstList secondList

  log $ show $ firstList
  log $ show $ secondList
  log $ show $ combinedList

  log $ show $ findIndex (_ < 5) firstList
  log $ show $ findLastIndex (_ == 12) firstList
  log $ show $ unzip combinedList
  log $ show $ fst $ unzip combinedList
  log $ show $ snd $ unzip combinedList
  log $ show $ filter (_ > 4) firstList
  log $ show $ tailRecursionFilter (_ > 4) firstList
  log $ show $ take 2 secondList
  log $ show $ tailRecursionTake 2 secondList
