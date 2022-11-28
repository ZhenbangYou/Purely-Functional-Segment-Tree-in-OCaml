module SegmentTree =
    type treeNode =
        | Leaf of leafType
        | Internal of internalType

    and internalType =
        { leftChild: treeNode
          rightChild: treeNode
          low: int
          high: int
          sum: int
          addend: int }

    and leafType = { value: int }

    type t = treeNode

    let create (numbers: int []) =
        let rec create' low high =
            if low = high then
                (Leaf { value = numbers[low] }, numbers[low])
            else
                let mid = (low + high) / 2
                let leftChild, lsum = create' low mid
                let rightChild, rsum = create' (mid + 1) high

                (Internal
                    { leftChild = leftChild
                      rightChild = rightChild
                      low = low
                      high = high
                      sum = lsum + rsum
                      addend = 0 },
                 lsum + rsum)

        let res, _ = create' 0 (numbers.Length - 1)
        res

    let queryRange (node: t) (low: int) (high: int) : int =
        let rec queryRange' (node: treeNode) (low: int) (high: int) (accAddend: int) : int =
            match node with
            | Leaf { value = value } -> accAddend + value
            | Internal { leftChild = leftChild
                         rightChild = rightChild
                         low = l
                         high = h
                         sum = sum
                         addend = addend } ->
                let mid = (l + h) / 2

                if low = l && high = h then
                    sum + ((high - low + 1) * accAddend)
                else if high <= mid then
                    queryRange' leftChild low high (accAddend + addend)
                else if low > mid then
                    queryRange' rightChild low high (accAddend + addend)
                else
                    queryRange' leftChild low mid (accAddend + addend)
                    + queryRange' rightChild (mid + 1) high (accAddend + addend)

        queryRange' node low high 0

    let rec updateRange (node: t) (low: int) (high: int) (delta: int) =
        match node with
        | Leaf { value = value } -> Leaf { value = value + delta }
        | Internal { leftChild = leftChild
                     rightChild = rightChild
                     low = l
                     high = h
                     sum = sum
                     addend = addend } ->
            let mid = (l + h) / 2

            if low = l && high = h then
                Internal
                    { leftChild = leftChild
                      rightChild = rightChild
                      low = l
                      high = h
                      sum = sum + ((high - low + 1) * delta)
                      addend = addend + delta }
            else if high <= mid then
                Internal
                    { leftChild = updateRange leftChild low high delta
                      rightChild = rightChild
                      low = l
                      high = h
                      sum = sum + ((high - low + 1) * delta)
                      addend = addend }
            else if low > mid then
                Internal
                    { leftChild = leftChild
                      rightChild = updateRange rightChild low high delta
                      low = l
                      high = h
                      sum = sum + ((high - low + 1) * delta)
                      addend = addend }
            else
                Internal
                    { leftChild = updateRange leftChild low mid delta
                      rightChild = updateRange rightChild (mid + 1) high delta
                      low = l
                      high = h
                      sum = sum + ((high - low + 1) * delta)
                      addend = addend }
