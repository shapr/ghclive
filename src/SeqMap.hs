{-
  A sequence with lookuppable keys
-}
module SeqMap ( SeqMap
              , fromList
              , toList
              , toListFromTo
              , toListTo
              , toListFrom
              , values
              , keys
              , singleton
              , remove
              , removeAll
              , null
              , size
              , lookup
              , insertAfter
              , insertBefore
              , adjust
              , first
              , last
              , prev
              , next
              , (|>)
              , (<|)
              ) where

import           Prelude hiding (lookup, last, null)
import qualified Prelude

import           Control.Monad ((>=>))

import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe (fromJust, isJust)
import           Data.Monoid
import qualified Data.Set as S
import           Data.Set (Set)

data SeqMap k v = SeqMapM k k (Map k (SM k v))
                | SeqMapE
  deriving Show

data SM k v = SM
    { smVal :: v
    , smPrev :: Maybe k
    , smNext :: Maybe k
    } deriving Show

null SeqMapE = True
null _       = False

size SeqMapE         = 0
size (SeqMapM _ _ m) = M.size m

singleton :: k -> v -> SeqMap k v
singleton k v = SeqMapM k k (M.singleton k $ SM v Nothing Nothing)

fromList :: Ord k => [(k,v)] -> SeqMap k v
fromList [] = SeqMapE
fromList xs =
    let xs'  = nubBy' fst xs
        vs   = zipWith3 (\(k,v) p n -> (k, SM v p n)) xs'
                        (Nothing : map (Just . fst) xs') (map (Just . fst) (tail xs') ++ [Nothing])
    in  SeqMapM (fst . head $ xs') (fst . Prelude.last $ xs') (M.fromList vs)

toList :: Ord k => SeqMap k v -> [(k,v)]
toList SeqMapE = []
toList (SeqMapM f _ m) = go (Just f)
    where
      go (Just k) = let v = fromJust (M.lookup k m) in (k, smVal v) : go (smNext v)
      go Nothing  = []


toListFrom :: Ord k => k -> SeqMap k v -> [(k,v)]
toListFrom from sm@(SeqMapM _ l _) = toListFromTo from l sm
toListFrom _    _                  = []

toListTo :: Ord k => k -> SeqMap k v -> [(k,v)]
toListTo to sm@(SeqMapM f _ _) = toListFromTo f to sm
toListTo _  _                  = []

toListFromTo :: Ord k => k -> k -> SeqMap k v -> [(k,v)]
toListFromTo from to (SeqMapM _ _ m)
    | isJust (M.lookup from m) = go (Just from)
    | otherwise                = []
  where
    go (Just k) = let next = if k == to then [] else go (smNext v)
                      v    = fromJust (M.lookup k m)
                  in (k, smVal v) : next

    go Nothing  = []
toListFromTo _ _ SeqMapE = []

values :: Ord k => SeqMap k v -> [v]
values m = map snd (toList m)

keys :: Ord k => SeqMap k v -> [k]
keys m = map fst (toList m)

removeAll :: Ord k => [k] -> SeqMap k v -> SeqMap k v
removeAll ks m = foldl' (flip remove) m ks

remove :: Ord k => k -> SeqMap k v -> SeqMap k v
remove _ SeqMapE = SeqMapE
remove k sm@(SeqMapM f l m)
    | M.null m'                       = SeqMapE
    | Just (SM v p n) <- M.lookup k m = let (f', adjustp) = if f == k
                                                             then (fromJust n, id)
                                                             else (f, M.adjust (setNext n) (fromJust p))
                                            (l', adjustn) = if l == k
                                                             then (fromJust p, id)
                                                             else (l, M.adjust (setPrev p) (fromJust n))
                                        in SeqMapM f' l' (adjustp . adjustn $ m')
    | otherwise                       = sm
  where
    m' = M.delete k m

lookup :: Ord k => k -> SeqMap k v -> Maybe v
lookup k (SeqMapM _ _ m) = fmap smVal (M.lookup k m)
lookup _ _               = Nothing

first :: SeqMap k v -> Maybe k
first (SeqMapM f _ _) = Just f
first _               = Nothing

last :: SeqMap k v -> Maybe k
last (SeqMapM _ l _) = Just l
last _               = Nothing

next :: Ord k => k -> SeqMap k v -> Maybe k
next k = smLookup k >=> smNext

prev :: Ord k => k -> SeqMap k v -> Maybe k
prev k = smLookup k >=> smPrev

smLookup :: Ord k => k -> SeqMap k v -> Maybe (SM k v)
smLookup k (SeqMapM _ _ m) = M.lookup k m
smLookup _ _               = Nothing


-- | insert (k,v) before k0, no-op if k0 is not present, if k == k0 then the value is just replaced with v
insertBefore :: Ord k => k -> (k,v) -> SeqMap k v -> SeqMap k v
insertBefore k0 (k,v) sm@(SeqMapM f l m)
                        | k == k0                              = SeqMapM f l $ M.adjust (setVal v) k m
                        | Just (SM v0 p0 n0) <- M.lookup k0 m' =
                            let m'' = case p0 of
                                        Nothing -> M.insert k0 (SM v0 (Just k) n0) .
                                                   M.insert k  (SM v  Nothing (Just k0))  $ m'
                                        Just p  -> M.adjust (setNext (Just k)) p .
                                                   M.insert k0 (SM v0 (Just k) n0) .
                                                   M.insert k  (SM v  (Just p) (Just k0)) $ m'
                            in SeqMapM (if isJust p0 then f else k) l m''
                        | otherwise                            = sm
    where
      m' = M.delete k m
insertBefore _ _ m = m


-- | insert (k,v) after k0, no-op if k0 is not present
insertAfter :: Ord k => k -> (k,v) -> SeqMap k v -> SeqMap k v
insertAfter k0 (k,v) sm@(SeqMapM f l m)
                        | k == k0   = SeqMapM f l $ M.adjust (setVal v) k m
                        | Just (SM v0 p0 n0) <- M.lookup k0 m' =
                            let m'' = case n0 of
                                        Nothing -> M.insert k0 (SM v0 p0 (Just k)) .
                                                   M.insert k  (SM v  (Just k0) Nothing)  $ m'
                                        Just n  -> M.adjust (setPrev (Just k)) n .
                                                   M.insert k0 (SM v0 p0 (Just k)) .
                                                   M.insert k  (SM v  (Just k0) (Just n)) $ m'
                            in SeqMapM f (if isJust n0 then l else k) m''
                        | otherwise = sm
    where
      m' = M.delete k m
insertAfter _ _ m = m

adjust :: Ord k => (v -> v) -> k -> SeqMap k v -> SeqMap k v
adjust fun k (SeqMapM f l m) = SeqMapM f l (M.adjust (\(SM v p n) -> (SM (fun v) p n)) k m)
adjust _ _ SeqMapE         = SeqMapE

setVal  v (SM _ p n) = SM v p n
setPrev p (SM v _ n) = SM v p n
setNext n (SM v p _) = SM v p n

-- monoid instance removes duplicate keys from second part first
instance Ord k => Monoid (SeqMap k v) where
  mempty  = SeqMapE
  mappend sm0@(SeqMapM f0 l0 m0) sm1 =
    case removeAll (keys sm0) sm1 of
      SeqMapE          -> sm0
      SeqMapM f1 l1 m1 -> SeqMapM f0 l1 ( M.adjust (setNext (Just f1)) l0 .
                                          M.adjust (setPrev (Just l0)) f1 $
                                          M.union m0 m1
                                        )
  mappend SeqMapE sm = sm

-- 'tip' operators append a single value, if key exists somewhere else, it's removed first
(k,v) <| m = singleton k v <> remove k m

m |> (k,v) = remove k m <> singleton k v

--
nubBy' :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
nubBy' f xs = go xs S.empty
    where
      go [] _     = []
      go (x:xs) s | fx `S.member` s = go xs s
                  | otherwise       = x : go xs (S.insert fx s)
        where
          fx = f x

