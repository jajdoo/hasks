

module RBTree (
        RBTree,
        newTree,
        construct,
        treeInsert,
        forDot
) where

import Data.List

data NodeColor = Red | Black deriving(Eq) 

data RBTree a = Empty |
                Branch a NodeColor (RBTree a) (RBTree a)

-- new leaf ----------------------------------
redLeaf :: (Eq a, Ord a) =>  a -> RBTree a
redLeaf x = Branch x Red Empty Empty


-- new tree ----------------------------------
newTree :: (Eq a, Ord a) =>  a -> RBTree a
newTree root = Branch root Black Empty Empty


-- insert ------------------------------------    
treeInsert :: (Eq a, Ord a) => RBTree a -> a -> RBTree a
treeInsert Empty x = redLeaf x
treeInsert (Branch y c l r) x 
        | x<y  = Branch y c (treeInsert l x) r
        | x>y  = Branch y c l (treeInsert r x) 
        | x==y = Branch x c l r

-- construct a tree --------------------------
construct :: (Eq a, Ord a) => RBTree a -> [a] -> RBTree a
construct = foldl' treeInsert


-- print nice --------------------------------
forDot :: Show a => RBTree a -> String
forDot t = "digraph tree {\n" ++ forDot1 t "r" "" ++ "\n}" where
    forDot1 Empty _ _ = ""
    forDot1 (Branch y Red l r ) pre post = 
        let nextpre = "\""++(Prelude.show y) ++"\""
        in  
        "node [style=filled,color=red]; \n"++
        pre ++"->"++ nextpre ++ post ++ ";\n" ++
        forDot1 l nextpre " [label=\"l\"]" ++
        forDot1 r nextpre " [label=\"r\"]" 
                                    
    forDot1 (Branch y Black l r ) pre post = 
        let nextpre = "\""++(Prelude.show y) ++"\""
        in  
        "node [style=filled,color=lightgray]; \n"++
        pre ++"->"++ nextpre ++ post ++ ";\n" ++
        forDot1 l nextpre " [label=\"l\"]"  ++
        forDot1 r nextpre " [label=\"r\"]"
                                    