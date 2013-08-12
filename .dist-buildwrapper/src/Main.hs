--{-# LANGUAGE CPP, TemplateHaskell #-}
--{-# OPTIONS_GHC -O2 #-}
--{-# OPTIONS_GHC -prof #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Data.Time.Clock.POSIX
import System.Random
import System.IO
--import MyAVLTreeH
import MyAVLTree
import Control.DeepSeq (deepseq)
--import MyTree

-- rands -------------------------------------
rands :: Int -> Int -> Int -> Int -> [Int]
rands n low high seed = take n $ randomRs (low, high) (mkStdGen seed)


-- construct a string for gprahviz's dot -----
writeStrToFile :: String -> String -> IO ()
writeStrToFile filepath str = do
    outh <- openFile filepath WriteMode
    hPutStrLn outh str
    hClose outh


-- main --------------------------------------
main :: IO ()
main = do
    --startSeed <- round `fmap` getPOSIXTime
    
    start <- getPOSIXTime
    --let t1 = construct Empty ( rands 20 1 100 654654 )
    let seed = 9843985
    let ma = 9999999
    --print ( rands 999999 1 ma seed )
    let t = construct Empty ( rands ma  1 ma seed ) 
    end <- t `seq` getPOSIXTime
    print (end - start)
    --writeStrToFile "C://Users//Ni//Desktop//graph//tree1.dot"  $ forDot $ construct Empty ( rands ma 1 ma seed )
    {-
    writeStrToFile "C://Users//Ni//Desktop//graph//tree1.dot"  $ forDot $ construct Empty ( rands 1  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree2.dot"  $ forDot $ construct Empty ( rands 2  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree3.dot"  $ forDot $ construct Empty ( rands 3  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree4.dot"  $ forDot $ construct Empty ( rands 4  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree5.dot"  $ forDot $ construct Empty ( rands 5  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree6.dot"  $ forDot $ construct Empty ( rands 6  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree7.dot"  $ forDot $ construct Empty ( rands 7  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree8.dot"  $ forDot $ construct Empty ( rands 8  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree9.dot"  $ forDot $ construct Empty ( rands 9  1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree10.dot" $ forDot $ construct Empty ( rands 10 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree11.dot" $ forDot $ construct Empty ( rands 11 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree12.dot" $ forDot $ construct Empty ( rands 12 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree13.dot" $ forDot $ construct Empty ( rands 13 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree14.dot" $ forDot $ construct Empty ( rands 14 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree15.dot" $ forDot $ construct Empty ( rands 15 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree16.dot" $ forDot $ construct Empty ( rands 16 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree17.dot" $ forDot $ construct Empty ( rands 17 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree18.dot" $ forDot $ construct Empty ( rands 18 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree19.dot" $ forDot $ construct Empty ( rands 19 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree20.dot" $ forDot $ construct Empty ( rands 20 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree21.dot" $ forDot $ construct Empty ( rands 21 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree22.dot" $ forDot $ construct Empty ( rands 22 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree23.dot" $ forDot $ construct Empty ( rands 23 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree24.dot" $ forDot $ construct Empty ( rands 24 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree25.dot" $ forDot $ construct Empty ( rands 25 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree26.dot" $ forDot $ construct Empty ( rands 26 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree27.dot" $ forDot $ construct Empty ( rands 27 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree28.dot" $ forDot $ construct Empty ( rands 28 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree29.dot" $ forDot $ construct Empty ( rands 29 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree30.dot" $ forDot $ construct Empty ( rands 30 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree31.dot" $ forDot $ construct Empty ( rands 31 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree32.dot" $ forDot $ construct Empty ( rands 32 1 ma seed )
    writeStrToFile "C://Users//Ni//Desktop//graph//tree33.dot" $ forDot $ construct Empty ( rands 33 1 ma seed )
    
    end <- getPOSIXTime
    print (end - start)
    -}