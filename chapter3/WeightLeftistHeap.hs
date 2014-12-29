module WeightLeftistHeap (WeightLeftistHeap, fromList) where

import Heap
import Data.Monoid
import Data.Foldable

data WeightLeftistHeap a = E | T a (WeightLeftistHeap a) (WeightLeftistHeap a)
    deriving Show

instance Heap WeightLeftistHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    merge h E = h
    merge E h = h
    merge h1@(T x a b) h2@(T y _ _)
      | x <= y    = makeT x a b h2
      | otherwise = merge h2 h1
        where
            makeT x E E h = T x h E
            makeT x E b h = makeT x b E h
            makeT x a@(T y _ _) b h@(T z _ _)
              | y < z     = T x (merge h a) b
              | otherwise = T x a (merge h b)

    insert x h = merge h $ T x E E

    findMin (T x _ _) = x

    deleteMin (T _ a b) = merge a b

instance Ord a => Monoid (WeightLeftistHeap a) where
    mempty = E
    mappend = merge

fromList :: Ord a => [a] -> WeightLeftistHeap a
fromList as =
    fold $ map (\x -> T x E E) as

main :: IO ()
main = do
    let a = T 1 (T 8 E E) (T 2 (T 5 E E) (T 4 E E))
        b = T 3 (T 7 E E) E
        c = T 1 (T 8 E E) (T 2 E E)
        d = T 9 (T 10 E E) E
    print $ merge a b
    -- T 1 (T 8 E E) (T 2 (T 5 E E) (T 3 (T 7 E E) (T 4 E E)))
    print $ merge c d
    -- T 1 (T 8 (T 9 (T 10 E E) E) E) (T 2 E E)
    print $ merge (merge a b) (merge c d)
    -- T 1 (T 8 E E) (T 1 (T 8 (T 9 (T 10 E E) E) E) (T 2 (T 5 E E) (T 2 (T 3 (T 7 E E) (T 4 E E)) E)))
