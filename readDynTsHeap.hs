
module Main where

import RndDynTsHeap
import EFheap
import Data.FingerTree
import System.Environment
import System.IO


main = do
    args     <- getArgs
    let nox        = read     (args !! 0) :: Int 
    contents <- readFile (args !! 1)
    let sno        = read (takeWhile (/='\n') contents) :: Int
    let contents'  = drop 1 $ dropWhile (/='\n') contents
    let no         = read (takeWhile (/='\n') contents') :: Int
    let contents2  = drop 1 $ dropWhile (/='\n') contents'
    let fs         = read (takeWhile (/='\n') contents2) :: Int 
    let contents3  = drop 1 $ dropWhile (/='\n') contents2
    let ts         = read (takeWhile (/='\n') contents3) :: Int 
    let contents4  = drop 1 $ dropWhile (/='\n') contents3
    let rs         = read (takeWhile (/='\n') contents4) :: Int 
    let contents5  = drop 1 $ dropWhile (/='\n') contents4 
    let ops        = read (takeWhile (/='\n') contents5) :: [Int]
    let lops       = length ops
    let contents6  = drop 1 $ dropWhile (/='\n') contents5 
    let nds        = read contents6 :: [Int]
    let initF = rndForest    (1,fs) ts rs emptyForest

    let forest@(ForestEF nnodes size ft) = dynOps (take nox ops) nds initF
    let (f' :< _) = viewl (getFT ft)
    let (f  :< _) = viewl f' 
    let (_ :> l') = viewr (getFT ft)
    let (_ :> l ) = viewr l'
    putStrLn $  "\nNodes: " ++ show nnodes ++ ", Size: " ++ show size
             ++ "\nFirst: " ++ show f ++ "; Last: " ++ show l


dynOps :: Ord a
       => [Int]                                -- Operations link | cut 
       -> [a]                                  -- [random] nodes 
       -> ForestEF a                           -- from forest 
       -> ForestEF a                           -- to   forest 
dynOps []       _                   forest = forest 
dynOps _        []                  forest = forest 
dynOps _        [node]              forest = forest 
dynOps (op:ops) (node1:node2:nodes) forest = 
    let forest' = case op of
          1 -> (link node1 node2) forest
          2 -> (cut  node1 node2) forest
    in  dynOps ops nodes forest' 
