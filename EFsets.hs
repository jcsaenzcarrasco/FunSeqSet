{-#  LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module EFsets where 

import Data.FingerTree ((<|),(|>),(><),search,empty,Measured,ViewL(EmptyL,(:<)),SearchResult(Position),FingerTree,viewl,measure)
import qualified Data.FingerTree as FT
import qualified Data.Set as S
import Data.Maybe (fromJust) 
import GHC.Exts (IsList(..))
import Data.Monoid ((<>))

-- ************************************************************
--    FT (Set (a,a))  (a,a) 

type TreeEF   a = FingerTree (S.Set (a,a)) (a,a) 
data ForestEF a = ForestEF NumNodes ForestSize (FT a)

type NumNodes   = Int -- Also edges of the form (v,v)
type ForestSize = Int -- Also is the total number of edges including NumNodes

newtype FT a    = FT {getFT :: (FingerTree (S.Set (a,a)) (TreeEF a))} 
 deriving Show

emptyForest :: Ord a => ForestEF a  
emptyForest  = ForestEF 0 0 (FT FT.empty) 

emptyTree :: Ord a => TreeEF a 
emptyTree  = FT.empty 

instance (Ord a) => Measured (S.Set (a,a)) (a,a) where 
   measure (x,y) = S.insert (x, y) S.empty 

instance Show a => Show (ForestEF a) where
  show (ForestEF v s ft) =
    "FT " ++ show v ++ " " ++ show s ++ "\n" ++ show (getFT ft) 

-- ----------------------------------------------------------
--           ROOT of tree 

root :: Ord a => TreeEF a -> Maybe a  
root tree = case viewl tree of
  EmptyL   -> Nothing
  x :< _   -> Just $ fst x

-- ----------------------------------------------------------
--           REroot of tree 

reroot :: Ord a => TreeEF a -> a -> TreeEF a 
reroot tree vertex = case (FT.search pred tree) of
   Position left _ right -> root <| (right >< left)
   _                     -> tree
 where root          = (vertex,vertex)
       pred before _ = (S.member root) before

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
-- 
--            CONNECTED ( u, v ) in same tree ? IN WHICH tour ?

nodeIn :: Ord a => a -> ForestEF a -> Maybe (TreeEF a, a) 
nodeIn v (ForestEF _ _ ft) = 
 case FT.search pred (getFT ft) of 
  Position _ tree _ ->
     case (root tree) of
       Nothing    -> Nothing 
       Just rootT -> Just (tree, rootT) 
  _                 -> Nothing     -- node v not in forest
 where
   pred before _ = (S.member (v,v)) before 


-- | MAIN function regarding connectivity topic
-- | Are these two nodes connected in the provided forest?
conn :: Ord a => a -> a -> ForestEF a -> Bool
conn x y f =
 case (nodeIn x f, nodeIn y f) of 
  (Nothing          , _           ) -> False
  (_                , Nothing     ) -> False
  (Just (tx,rx)     , Just (ty,ry)) ->
                     if rx == ry  then True
                                  else False

conn' :: Ord a => a -> a -> ForestEF a -> ForestEF a 
conn' x y f =
 case (nodeIn x f, nodeIn y f) of 
  (Nothing          , _           ) -> f
  (_                , Nothing     ) -> f
  (Just (tx,rx)     , Just (ty,ry)) ->
                     if rx == ry  then f
                                  else f

-- ASKs for connectivity provided a [random] node list
-- applied to a [random] forest to get a list of results
connList :: Ord a => [a] -> ForestEF a -> [Bool] 
connList []       f = []
connList [x]      f = []
connList (x:y:ys) f = conn x y f : connList ys f 


connected :: Ord a => a -> a -> ForestEF a
          -> (Bool, Maybe (TreeEF a, a, TreeEF a, a) ) 
connected x y f = 
 case (nodeIn x f, nodeIn y f) of 
  (Nothing          , _           ) -> (False, Nothing) 
  (_                , Nothing     ) -> (False, Nothing) 
  (Just (tx,rx)     , Just (ty,ry)) -> if rx == ry 
                                   then (True,  Just(tx,rx,tx,rx))  
                                   else (False, Just(tx,rx,ty,ry))

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                           L I N K  (for trees and forest)

linkTree :: Ord a => a -> TreeEF a -> a -> TreeEF a -> Maybe (TreeEF a) 
linkTree u tu v tv = case (edgeIn (u,u) tu, edgeIn (v,v) tv) of
  (False, _    ) -> Nothing
  (_    , False) -> Nothing 
  (True , True ) -> Just $
    let from = reroot tu u
        (Position left _ right) = FT.search pred tv
    in  ((left |> (v,v)) |> (v,u)) >< from >< ((u,v) <| right)
 where
   pred before _ = (S.member (v,v)) before


-- for a really DENSE forest (trend is few giant trees)
linkDEN :: Ord a => a -> a -> ForestEF a -> ForestEF a 
linkDEN x y forest@(ForestEF nnodes size ft) = 
    if (edgeIn (x,y) f) then forest
    else 
     case connected x y forest of 
      (False, Just (tx,rx,ty,ry)) -> case (linkTree x tx y ty) of
         Nothing     -> forest
         Just result -> linkAll result 
      _                           -> forest 
 where
    f                  = getFT ft 
    Position lf' _ rf' = FT.search predX f 
    Position lf  _ rf  = FT.search predY (lf' >< rf') 
    linkAll tree    = ForestEF nnodes (size+2) (FT $ tree <| (lf >< rf) )
    predX before _ = (S.member (x,x)) before 
    predY before _ = (S.member (y,y)) before 

-- for an AVERAGE and SPARSE forest
linkAVG :: Ord a => a -> a -> ForestEF a -> ForestEF a 
linkAVG x y forest@(ForestEF nnodes size ft) = 
     case connected x y forest of 
      (False, Just (tx,rx,ty,ry)) -> case (linkTree x tx y ty) of
         Nothing     -> forest
         Just result -> linkAll result 
      _                           -> forest 
 where
    f                  = getFT ft 
    Position lf' _ rf' = FT.search predX f 
    Position lf  _ rf  = FT.search predY (lf' >< rf') 
    linkAll tree    = ForestEF nnodes (size+2) (FT $ tree <| (lf >< rf) )
    predX before _ = (S.member (x,x)) before 
    predY before _ = (S.member (y,y)) before 


link :: Ord a => a -> a -> ForestEF a -> ForestEF a 
link x y forest@(ForestEF nnodes size ft)
  | x == y            = forest
  | size > (2*nnodes) = linkDEN x y forest
  | otherwise         = linkAVG x y forest


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                             C U T   (for trees and forest)
--

cutTree :: Ord a => a -> a -> TreeEF a -> Maybe (TreeEF a,TreeEF a) 
cutTree u v tree = case FT.search predUV tree of
 Position left _ right ->
   case (FT.search predVU left ) of
      Position leftL _ rightL ->           -- (v,u) is on the left 
        Just (rightL, leftL >< right)
      _              ->                    -- (v,u) is on the right
        case (FT.search predVU right) of
          Position leftR _ rightR ->
            Just (leftR, left >< rightR)
          _ -> Nothing -- >>>> BAD Formed tree since (v,u) is missing 
 _  -> Nothing  -- >>>>>>>>> BAD Formed tree since (u,v) is missing     
 where
   predUV before _ = (S.member (u,v)) before 
   predVU before _ = (S.member (v,u)) before 


-- for AVERAGE and SPARSE forest
cutAVG :: Ord a => a -> a -> ForestEF a -> ForestEF a 
cutAVG x y forest@(ForestEF nnodes size ft) = 
    if not (edgeIn (x,y) f) then forest
    else
      case tree of
        Nothing -> forest
        Just tx -> case (cutTree x y tx) of
          Nothing     -> forest 
          Just result -> buildForest result  
 where
    f                   = getFT ft
    buildForest (t2,t3) = ForestEF nnodes (size - 2) (FT $ t2 <| (t3 <| (lf >< rf)) ) 
    Position lf _ rf    = FT.search pred f
    pred before _       = (S.member (x,x)) before
    (_,tree)            = edgeInWith (x,y) forest


-- for really DENSE forest (trend is few giant trees) 
cutDEN :: Ord a => a -> a -> ForestEF a -> ForestEF a 
cutDEN x y forest@(ForestEF nnodes size ft) = 
    if not exists then forest
    else
      case tree of
        Nothing  -> forest 
        Just tx  -> case (cutTree x y tx) of
          Nothing     -> forest 
          Just result -> buildForest result  
 where
    f                   = getFT ft 
    buildForest (t2,t3) = ForestEF nnodes (size - 2) (FT $ t2 <| (t3 <| (lf >< rf)) )
    Position lf _ rf    = FT.search pred f
    pred before _       = (S.member (x,x)) before
    (exists,tree)       = edgeInWith (x,y) forest


cut :: Ord a => a -> a -> ForestEF a -> ForestEF a 
cut x y forest@(ForestEF nnodes size ft)
  | x == y            = forest
  | size > (2*nnodes) = cutDEN x y forest
  | otherwise         = cutAVG x y forest

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--            HELPER functions  (for trees and forest)
--

-- Test membership of an edge (or vertex when (x,x)) in monoidal set
-- First type 'a' is usually a pair, 'b' can be either a pair or a tree
edgeIn :: (Measured (S.Set a) b, Ord a)
       => a
       -> FingerTree (S.Set a) b
       -> Bool
edgeIn e setFT = case (FT.search pred setFT) of
  Position _ _ _ -> True 
  _              -> False
 where
   pred before _ = (S.member e) before 


-- Test membership of an edge (or vertex when (x,x)) in monoidal set
-- ALSO return the second value in the search: the corresponding tree
-- First type 'a' is usually a pair, 'b' can be either a pair or a tree
edgeInWith :: (Ord a)
           => (a,a)
           -> ForestEF a
           -> (Bool, Maybe (TreeEF a)) 
edgeInWith e (ForestEF _ _ ft) = case (FT.search pred setFT) of
  Position _ x _ -> (True  , Just x ) 
  _              -> (False , Nothing) 
 where
   pred before _ = (S.member e) before
   setFT         = getFT ft 


toListFT :: (Ord a, Measured v a) => FingerTree v a -> [a]
toListFT ft = case (viewl ft) of
  EmptyL    -> []
  x :< rest -> x : toListFT rest

