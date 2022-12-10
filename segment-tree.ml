{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SegmentTree
  ( TreeNode,
    create,
    queryRange,
    updateRange,
  )
where

import Data.Sequence

data TreeNode a
  = Leaf a
  | Internal
      { leftChild :: TreeNode a,
        rightChild :: TreeNode a,
        low :: Int,
        high :: Int,
        childSum :: a,
        addend :: a
      }
  deriving (Show)

create' :: Num a => Seq a -> Int -> Int -> (TreeNode a, a)
create' numbers low high =
  if low == high
    then
      let element = numbers `index` low
       in (Leaf element, element)
    else
      let mid = (low + high) `div` 2
          (leftChild, lsum) = create' numbers low mid
          (rightChild, rsum) = create' numbers (mid + 1) high
          childSum = lsum + rsum
       in ( Internal
              { leftChild,
                rightChild,
                low,
                high,
                childSum,
                addend = 0
              },
            childSum
          )

create :: Num a => Seq a -> TreeNode a
create numbers =
  let (res, _) = create' numbers 0 (Data.Sequence.length numbers - 1)
   in res

queryRange' :: Num a => TreeNode a -> Int -> Int -> a -> a
queryRange' (Leaf v) _ _ accAddend = accAddend + v
queryRange' (Internal {..}) low' high' accAddend =
  let mid = (low + high) `div` 2
   in case () of
        _
          | low == low' && high == high' -> childSum + fromIntegral (high' - low' + 1) * accAddend
          | high' <= mid -> queryRange' leftChild low' high' (accAddend + addend)
          | low' > mid -> queryRange' rightChild low' high' (accAddend + addend)
          | otherwise ->
              queryRange' leftChild low' mid (accAddend + addend)
                + queryRange' rightChild (mid + 1) high' (accAddend + addend)

queryRange :: Num a => TreeNode a -> Int -> Int -> a
queryRange node low high = queryRange' node low high 0

updateRange :: Num a => TreeNode a -> Int -> Int -> a -> TreeNode a
updateRange (Leaf v) _ _ delta = Leaf (v + delta)
updateRange node@(Internal {..}) low' high' delta =
  let mid = (low + high) `div` 2
      newChildSum = childSum + fromIntegral (high' - low' + 1) * delta
   in case () of
        _
          | low' == low && high' == high ->
              node
                { childSum = newChildSum,
                  addend = addend + delta
                }
          | high' <= mid ->
              node
                { leftChild = updateRange leftChild low high delta,
                  childSum = newChildSum
                }
          | low' > mid ->
              node
                { rightChild = updateRange rightChild low high delta,
                  childSum = newChildSum
                }
          | otherwise ->
              node
                { leftChild = updateRange leftChild low mid delta,
                  rightChild = updateRange rightChild (mid + 1) high delta,
                  childSum = newChildSum
                }
