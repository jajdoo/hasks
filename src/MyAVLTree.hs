-----------------------------------------------------------------------------
--
-- Module      :  MyAVLTree
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

module MyAVLTree (
    Tree (Empty, Branch),
    treeInsert,
    --treeDelete,
    --treeElem,
    treeDepth,
    construct,
    forDot
) where

import Data.List
import Data.Int

data Tree a =   Empty |
                Branch {    key     :: a,
                            balance :: Int8,
                            left    :: Tree a,
                            right   :: Tree a,
                            up      :: Bool    --used internally to stop updating balance
                       }
                deriving (Eq)

leaf :: (Ord a, Eq a) => a -> Tree a
leaf x = Branch x 0 Empty Empty True

-- insert ------------------------------------
treeInsert :: (Eq a, Ord a) => Tree a -> a -> Tree a
treeInsert Empty x  = leaf x
treeInsert (Branch y b l r _) x | x<y           = let nl@(Branch _ _ _ _ nlu) = treeInsert l x
                                                  in 
                                                        if nlu then if b==1 then roll $ Branch y 2       nl r False 
                                                                            else        Branch y (b + 1) nl r (b /= (-1)) 
                                                               else Branch y b nl r False
                                                  
                                | x>y           = let nr@(Branch _ _ _ _ nru) = treeInsert r x
                                                  in 
                                                        if nru then if b==(-1) then roll $ Branch y (-2)    l nr False 
                                                                               else        Branch y (b - 1) l nr (b /= 1) 
                                                               else Branch y b l nr False
                                                  
                                | otherwise     = Branch x b l r False


-- rolls -------------------------------------
roll :: (Eq a, Ord a) => Tree a -> Tree a
-- ll roll
roll (Branch y 2 (Branch ly 1 ll lr _) r _) = Branch ly 0 ll (Branch y 0 lr r False) False
-- rr roll
roll (Branch y (-2) l (Branch ry (-1) rl rr _) _) = Branch ry 0 (Branch y 0 l rl False) rr False
-- lr rolls
roll (Branch y 2 (Branch ly (-1) ll (Branch lry lrb lrl lrr _) _) r _) = case lrb of 0  -> Branch lry 0 (Branch ly 0 ll lrl False) (Branch y   0  lrr r False) False
                                                                                     1  -> Branch lry 0 (Branch ly 0 ll lrl False) (Branch y (-1) lrr r False) False
                                                                                     -1 -> Branch lry 0 (Branch ly 1 ll lrl False) (Branch y   0  lrr r False) False
-- rl rolls
roll (Branch y (-2) l (Branch ry 1 (Branch rly rlb rll rlr _) rr _) _) = case rlb of 0  -> Branch rly 0 (Branch y 0 l rll False) (Branch ry   0  rlr rr False) False
                                                                                     1  -> Branch rly 0 (Branch y 0 l rll False) (Branch ry (-1) rlr rr False) False
                                                                                     -1 -> Branch rly 0 (Branch y 1 l rll False) (Branch ry   0  rlr rr False) False


-- construct a tree --------------------------
construct :: (Eq a, Ord a) => Tree a -> [a] -> Tree a
construct = foldl' treeInsert
        
-- print nice --------------------------------
forDot :: Show a => Tree a -> String
forDot t = "digraph tree {\n" ++ forDot1 t "r" "" ++ "\n}" where
    forDot1 Empty _ _ = ""
    forDot1 (Branch y b l r _) pre post = let nextpre = "\""++(Prelude.show y) ++ ","++(Prelude.show b) ++"\""
                                          in 
                                          pre ++"->"++ nextpre ++ post ++ ";\n" ++
                                          (forDot1 l nextpre " [label=\"l\"]" ) ++
                                          (forDot1 r nextpre " [label=\"r\"]" )


-- depth -------------------------------------
treeDepth :: (Eq a, Ord a) => Tree a -> Int
treeDepth Empty = 0
treeDepth (Branch _ _ l r _) = max (1 + treeDepth l) (1 + treeDepth r)

{-

treeInsert :: (Eq a, Ord a) => Tree a -> a -> Tree a
treeInsert Empty x  = leaf x
treeInsert (Branch y b l r) x | x<y = Branch { key=y, left=(treeInsert l x), right=r }
                              | x>y = Branch { key=y, left=l,                right=(treeInsert r x) }
                              | otherwise = leaf x


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

-}
