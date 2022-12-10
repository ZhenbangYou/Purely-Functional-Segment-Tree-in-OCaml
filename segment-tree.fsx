module SegmentTree =
    type treeNode<'a> =
        | Leaf of leafType<'a>
        | Internal of internalType<'a>

    and internalType<'a> =
        { leftChild: treeNode<'a>
          rightChild: treeNode<'a>
          low: int
          high: int
          sum: 'a
          addend: 'a }

    and leafType<'a> = { value: 'a }

    type t<'a> = treeNode<'a>

    let inline create (numbers: ^a []) (zeroValue: ^a) : treeNode< ^a > =
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
                      addend = zeroValue },
                 lsum + rsum)

        let res, _ = create' 0 (numbers.Length - 1)
        res

    let inline queryRange (node: treeNode< ^a >) (low: int) (high: int) (zeroValue: ^a) : ^a =
        let rec queryRange' (node: treeNode< ^a >) (low: int) (high: int) (accAddend: ^a) : ^a =
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
                    sum
                    + (List.reduce (+) [ for _ in low..high -> accAddend ])
                else if high <= mid then
                    queryRange' leftChild low high (accAddend + addend)
                else if low > mid then
                    queryRange' rightChild low high (accAddend + addend)
                else

                    queryRange' leftChild low mid (accAddend + addend)
                    + queryRange' rightChild (mid + 1) high (accAddend + addend)

        queryRange' node low high zeroValue

    let rec inline updateRange (node: treeNode< ^a >) (low': int) (high': int) (delta: ^a) : treeNode< ^a > =
        match node with
        | Leaf { value = value } -> Leaf { value = value + delta }
        | Internal { leftChild = leftChild
                     rightChild = rightChild
                     low = low
                     high = high
                     sum = sum
                     addend = addend } ->
            let mid = (low + high) / 2

            let newSum =
                sum
                + (List.reduce (+) [ for _ in low'..high' -> delta ])

            if low' = low && high' = high then
                Internal
                    { leftChild = leftChild
                      rightChild = rightChild
                      low = low
                      high = high
                      sum = newSum

                      addend = addend + delta }
            else if high' <= mid then
                Internal
                    { leftChild = updateRange leftChild low high delta
                      rightChild = rightChild
                      low = low
                      high = high
                      sum = newSum
                      addend = addend }
            else if low' > mid then
                Internal
                    { leftChild = leftChild
                      rightChild = updateRange rightChild low high delta
                      low = low
                      high = high
                      sum = newSum
                      addend = addend }
            else
                Internal
                    { leftChild = updateRange leftChild low mid delta
                      rightChild = updateRange rightChild (mid + 1) high delta
                      low = low
                      high = high
                      sum = newSum
                      addend = addend }
