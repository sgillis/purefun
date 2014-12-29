module Heap (Heap(..)) where

class Heap h where
    empty     :: Ord a => h a
    isEmpty   :: Ord a => h a -> Bool
    merge     :: Ord a => h a -> h a -> h a
    insert    :: Ord a => a -> h a -> h a
    findMin   :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a
