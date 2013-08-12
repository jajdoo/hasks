

module MyAVLTreeH (
    Tree,
    treeInsert,
    --treeDelete,
    --treeElem,
    construct,
    forDot
) where

import Data.List

data Tree a =   Empty |
                Branch { key         :: a,
                         height      :: Int,
                         left        :: Tree a,
                         right       :: Tree a
                       }
                deriving (Eq)

--deadend :: (Ord a, Eq a) => Tree a
--deadend = Empty 0

leaf :: (Ord a, Eq a) => a -> Tree a
leaf x = Branch x 0 Empty Empty

-- insert ------------------------------------
treeInsert :: (Eq a, Ord a) => Tree a -> a -> Tree a
treeInsert Empty x = leaf x
treeInsert (Branch y h l r) x   | x<y = let nl = treeInsert l x        -- nl = new left
                                            nh = max h $ 1 + height nl -- nh = new height
                                        in  roll (Branch y nh nl r)
                                         
                                | x>y = let nr = treeInsert r x        -- nr = new right
                                            nh = max h $ 1 + height nr -- nh = new height
                                        in roll (Branch y nh l nr)
                                         
                                | x==y = Branch y h l r
        where
        roll :: (Eq a, Ord a) => Tree a -> Tree a
        roll (Branch y h l r) = (Branch y h l r)
        roll t = t
        
                where 
                rollLL (Branch y h (Branch ly lh ll lr) r) = Branch ly (h-1) ll (Branch y (lh-1) lr r)
                rollRR (Branch y h l (Branch ry rh rl rr)) = t 
                rollLR t = t
                rollRL t = t

-- height -----------------------------------
--treeHeight :: Tree a -> Int 
--treeHeight Empty = 0
--treeHeight (Branch _ h _ _) = h



-- construct a tree --------------------------
construct :: (Eq a, Ord a) => [a] -> Tree a
construct = foldl' treeInsert Empty


-- print nice --------------------------------
forDot :: Show a => Tree a -> String
forDot t = "digraph tree {\n" ++ forDot1 t "r" "" ++ "\n}" where
    forDot1 Empty  _ _ = ""
    forDot1 (Branch y b l r) pre post = let nextpre = "\""++(Prelude.show y) ++ ","++(Prelude.show b) ++"\""
                                        in 
                                        pre ++"->"++ nextpre ++ post ++ ";\n" ++
                                        (forDot1 l nextpre " [label=\"l\"]" ) ++
                                        (forDot1 r nextpre " [label=\"r\"]" )


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
