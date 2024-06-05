{- Instancia para el tipo listas -}

module ListSeq where

import Seq
import Par


auxTab f 0 = id
auxTab f n = let (a,b) = auxTab f (n-1) ||| (f (n-1) :)
          in a.b

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = auxTab f n []

-----------------------------------

auxMap f []     = id
auxMap f (x:xs) = let (a,b) = (f x :) ||| auxMap f xs
                  in a.b

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = auxMap f xs []

-----------------------------------

auxFilter f []     = id
auxFilter f (x:xs) = let (a,b) = (if f x then (x:) else id) ||| auxFilter f xs
                     in a.b

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = auxFilter f xs []

-----------------------------------

myAppend :: [a] -> [a] -> [a]
myAppend [] = id
myAppend (x:xs) = let (a,b) = (x:) ||| myAppend xs
                  in a.b

-- append = foldr (\ x -> (.) (x :)) id

-----------------------------------

myTake :: [a] -> Int -> [a]
myTake xs n = take n xs

-----------------------------------

myDrop :: [a] -> Int -> [a]
myDrop xs n = drop n xs

-----------------------------------

myShowt :: [a] -> TreeView a [a]
myShowt []  = EMPTY
myShowt [x] = ELT x
myShowt xs  = let l = length xs `div` 2
                  (a,b) = myTake xs l ||| myDrop xs l
              in NODE a b

-----------------------------------

myShowl :: [a] -> ListView a [a]
myShowl [] = NIL
myShowl (x:xs) = CONS x xs

-----------------------------------

myJoin :: [[a]] -> [a]
myJoin []          = []
myJoin [xs]        = xs
myJoin (xs:ys:xss) = let (a,b) = myAppend xs ys ||| myJoin xss
                     in myAppend a b

-----------------------------------

myReduce :: (a -> a -> a) -> a -> [a] -> a
myReduce f e xs = case myShowt xs of
  EMPTY    -> e
  ELT v    -> v
  NODE l r -> let (a,b) = myReduce f e l ||| myReduce f e r
              in f a b

-----------------------------------

contraer f [] = []
contraer f [x] = [x]
contraer f (x:y:xs) = let (a,b) = f x y ||| contraer f xs
                      in a:b

mezclar f [] [] = []
mezclar f [x] b@[x'] = b
mezclar f (x:y:xs) (x':xs') = let (a,b) = f x' x ||| mezclar f xs xs'
                              in x': a : b


myScan :: (a -> a -> a) -> a -> [a] -> ([a], a)
myScan f e []  = ([], e)
myScan f e [x] = ([e], f e x)
myScan f e s   = let (a,b) = myScan f e (contraer f s)
                 in (mezclar f s a, b)

-----------------------------------

instance Seq [] where
  emptyS     = []
  singletonS = (:[])
  lengthS    = length
  nthS       = (!!)
  tabulateS  = tabulate
  mapS       = myMap
  filterS    = myFilter
  appendS    = myAppend
  takeS      = myTake
  dropS      = myDrop
  showtS     = myShowt
  showlS     = myShowl
  joinS      = myJoin
  reduceS    = myReduce
  scanS      = myScan
  fromList   = id


