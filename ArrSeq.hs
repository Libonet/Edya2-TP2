{- Instancia para el tipo array -}

module ArrSeq where

import Arr ((!))
import qualified Arr as A

import Seq
import Par

myMap :: (a -> b) -> A.Arr a -> A.Arr b
myMap f xs = A.tabulate (\i -> f (xs ! i)) (A.length xs)

myFilter :: (a -> Bool) -> A.Arr a -> A.Arr a
myFilter f = A.flatten . myMap (\x -> if f x then singletonS x else emptyS)

--myAppend :: A.Arr a -> A.Arr a -> A.Arr a 
--myAppend a b = let 
--    (la, lb) = A.length a ||| A.length b
--    n = la + lb
--    in A.tabulate (\i -> if i < la then a ! i else b ! (i - la)) n

myAppend a b = A.flatten (A.fromList [a, b])

myTake :: A.Arr a -> Int -> A.Arr a
myTake xs n = A.subArray 0 n xs

myDrop :: A.Arr a -> Int -> A.Arr a
myDrop xs n = A.subArray n (A.length xs - n) xs

myShowt :: A.Arr a -> TreeView a (A.Arr a)
myShowt xs = case A.length xs of
    0 -> EMPTY
    1 -> ELT (xs ! 0)
    n -> NODE (myTake xs (div n 2)) (myDrop xs (div n 2))

myShowl :: A.Arr a -> ListView a (A.Arr a)
myShowl xs = case A.length xs of
    0 -> NIL
    _ -> CONS (xs ! 0) (myDrop xs 1)

myReduce :: (a -> a -> a) -> a -> A.Arr a -> a
myReduce f e xs = case myShowt xs of
  EMPTY    -> e
  ELT v    -> v
  NODE l r -> let (a,b) = myReduce f e l ||| myReduce f e r
              in f a b

--------------------------------------

contraer :: (a -> a -> a) -> A.Arr a -> A.Arr a
contraer f xs = case A.length xs of
  0 -> A.empty
  n -> let (con, isEven) = A.tabulate (\i -> f (xs ! (2*i)) (xs ! ((2*i)+1))) (div n 2) ||| even n
       in if isEven then con
          else myAppend con (A.fromList [xs ! (n-1)])

mezclar :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
mezclar f xs ys = case A.length xs ||| A.length ys of
  (lx, 0)  -> A.empty
  (1, ly)  -> ys
  (lx, ly) -> let (a,b) = f (xs ! 0) (ys ! 0) ||| mezclar f (myDrop xs 2) (myDrop ys 1)
              in myAppend (A.fromList [ys ! 0, a]) b

myScan :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
myScan f e xs = case A.length xs of
  0 -> (A.empty, e)
  1 -> (A.fromList [e], f e (xs ! 0))
  n -> let (a,b) = myScan f e (contraer f xs)
       in (mezclar f xs a, b)

instance Seq A.Arr where
  emptyS     = A.empty
  singletonS = A.fromList . (:[])
  lengthS    = A.length
  nthS       = (!)
  tabulateS  = A.tabulate
  mapS       = myMap
  filterS    = myFilter
  appendS    = myAppend
  takeS      = myTake
  dropS      = myDrop
  showtS     = myShowt
  showlS     = myShowl
  joinS      = A.flatten
  reduceS    = myReduce
  scanS      = myScan
  fromList   = A.fromList



