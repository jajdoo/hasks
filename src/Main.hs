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
--import MyAVLTree
import RBTree
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


helper :: Int -> RBTree Int -> IO ()
helper 0 _ = return ()
helper n t = do 
    --seed <- round `fmap` getPOSIXTime
    let ma = 100
    let ll = ( rands n 1 ma 2349824 ) 
    writeStrToFile ("C://Users//Ni//Desktop//graph//tree"++(show n)++".dot") $ forDot $ construct t ll
    helper (n-1) t 

-- main --------------------------------------
main :: IO ()
main = do
        let r = newTree 50 
        helper 20 r
    --startSeed <- round `fmap` getPOSIXTime
    
    {-    
    let t1 = construct Empty ( rands 20 1 100 654654 )
    seed <- round `fmap` getPOSIXTime
    let ma = 999999
    let ll = ( rands ma 1 ma seed ) 
    let t = construct Empty ll
    start <- getPOSIXTime
    end <- t `seq` getPOSIXTime
    writeStrToFile "C://Users//Ni//Desktop//graph//tree1.dot"  $ forDot $ construct Empty ( rands ma 1 ma seed )
    print (end - start)
    -}