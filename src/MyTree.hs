-----------------------------------------------------------------------------
--
-- Module      :  MyTree
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module MyTree (
    Tree (Empty, Branch),
    treeInsert,
    treeInsert',
    --treeInsert'',
    treeDelete,
    treeElem,
    treeDepth,
    construct,
    construct',
    forDot
) where

import Data.List
import Control.DeepSeq

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq)

leaf :: (Ord a, Eq a) => a -> Tree a
leaf x = Branch x Empty Empty

-- insert ------------------------------------
treeInsert :: (Eq a, Ord a) => Tree a -> a -> Tree a
treeInsert Empty x  = leaf x
treeInsert (Branch y l r) x | x<y = Branch y (treeInsert l x) r
                            | x>y = Branch y l (treeInsert r x)
                            | otherwise = Branch x l r

-- eager insert ------------------------------
treeInsert' :: (Eq a, Ord a) => Tree a -> a -> Tree a
treeInsert' Empty x  = leaf x
treeInsert' (Branch y l r) x | x<y = let l'=treeInsert l x in l' `seq` Branch y l' r
                             | x>y = let r'=treeInsert r x in r' `seq` Branch y l r'
                             | otherwise = Branch x l r

--very eager insert ------------------------------
--treeInsert'' :: (Eq a, Ord a) => Tree a -> a -> Tree a
--treeInsert'' Empty x  = leaf x
--treeInsert'' (Branch y l r) x | x<y = let l'=treeInsert l x in l' `deepseq` Branch y l' r
--                              | x>y = let r'=treeInsert r x in r' `deepseq` Branch y l r'
--                              | otherwise = Branch x l r

-- delete ------------------------------------
treeDelete :: (Eq a, Ord a) => Tree a -> a -> Tree a
treeDelete Empty _ = Empty
treeDelete (Branch y l r ) x    | y<x   = Branch y l (treeDelete r x)
                                | y>x   = Branch y (treeDelete l x) r
                                | y==x  = del' $ Branch y l r
    where
    -- if this Branch is a leaf dispose of it.
    -- if branch has only one child return the child (skip over).
    -- otherwise, replace this branch with its successor (the leftmost child of the right tree)
    --      successor will be extracted from its original location.
    del' ( Branch y Empty Empty )   = Empty
    del' ( Branch y Empty r )       = r
    del' ( Branch y l Empty )       = l
    del' ( Branch y l r )           = Branch ySucc l rWithout_ySucc

        where
        ( rWithout_ySucc, ySucc ) = leftmost r

            where
            leftmost ( Branch y Empty Empty )   = ( Empty, y )
            leftmost ( Branch y Empty r )       = ( r, y )
            leftmost ( Branch y l r )           = ( Branch y ll r, z ) where ( ll, z ) = leftmost l


-- search ------------------------------------
treeElem :: (Eq a, Ord a) => a -> Tree a -> Bool
treeElem x Empty  = False
treeElem x (Branch y l r) | x<y = treeElem x l
                          | x>y = treeElem x r
                          | x==y = True



-- depth -------------------------------------
treeDepth :: (Eq a, Ord a) => Tree a -> Int
treeDepth Empty = 0
treeDepth (Branch a l r) = max (1 + treeDepth l) (1 + treeDepth r)



-- print nice --------------------------------
instance Show a => Show (Tree a) where
show t = nicePrint t

nicePrint t = nicePrint1 t 1 where
    nicePrint1 :: Show a => Tree a -> Int -> String
    nicePrint1 Empty n = "Empty"
    nicePrint1 (Branch y l r) n =   let tabs = (take n (repeat '\t'))
                                        tabc = (take (n-1) (repeat '\t'))
                                    in
                                        "( "++(Prelude.show y)
                                        ++"\n"++ tabs ++ (nicePrint1 l (n+1))
                                        ++"\n"++ tabs ++ (nicePrint1 r (n+1))
                                        ++"\n"++ tabc ++")\n"


treeInorder Empty = []
treeInorder (Branch y l r) = (treeInorder l) ++ y:(treeInorder r)


forDot :: Show a => Tree a -> String
forDot t = "digraph tree {\n" ++ (forDot1 t "r" "") ++ "\n}" where
    forDot1 Empty _ _ = ""
    forDot1 (Branch y l r) pre post = pre ++"->"++(Prelude.show y) ++ post ++ ";\n" ++
                                      (forDot1 l (Prelude.show y) " [label=\"l\"]") ++
                                      (forDot1 r (Prelude.show y) " [label=\"r\"]")


-- construct a tree --------------------------
construct :: (Eq a, Ord a) => Tree a -> [a] -> Tree a
construct t xs = foldl' treeInsert t xs

-- eager construct a tree --------------------
construct' :: (Eq a, Ord a) => Tree a -> [a] -> Tree a
construct' t xs = foldl' treeInsert' t xs


