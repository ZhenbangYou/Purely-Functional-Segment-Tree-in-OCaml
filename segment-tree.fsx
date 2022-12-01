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

    let create (numbers: 'a []) (zeroValue: 'a) (add: 'a -> 'a -> 'a) =
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
                      sum = add lsum rsum
                      addend = zeroValue },
                 add lsum rsum)

        let res, _ = create' 0 (numbers.Length - 1)
        res

    let queryRange (node: t<'a>) (low: int) (high: int) (zeroValue: 'a) (add: 'a -> 'a -> 'a) : 'a =
        let rec queryRange' (node: treeNode<'a>) (low: int) (high: int) (accAddend: 'a) : 'a =
            match node with
            | Leaf { value = value } -> add accAddend value
            | Internal { leftChild = leftChild
                         rightChild = rightChild
                         low = l
                         high = h
                         sum = sum
                         addend = addend } ->
                let mid = (l + h) / 2

                if low = l && high = h then
                    add sum (List.reduce add [ for _ in low..high -> accAddend ])
                else if high <= mid then
                    queryRange' leftChild low high (add accAddend addend)
                else if low > mid then
                    queryRange' rightChild low high (add accAddend addend)
                else
                    add
                        (queryRange' leftChild low mid (add accAddend addend))
                        (queryRange' rightChild (mid + 1) high (add accAddend addend))

        queryRange' node low high zeroValue

    let rec updateRange (node: t<'a>) (low: int) (high: int) (delta: 'a) (zeroValue: 'a) (add: 'a -> 'a -> 'a) =
        match node with
        | Leaf { value = value } -> Leaf { value = add value delta }
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
                      sum = add sum (List.reduce add [ for _ in low..high -> delta ])
                      addend = add addend delta }
            else if high <= mid then
                Internal
                    { leftChild = updateRange leftChild low high delta zeroValue add
                      rightChild = rightChild
                      low = l
                      high = h
                      sum = add sum (List.reduce add [ for _ in low..high -> delta ])
                      addend = addend }
            else if low > mid then
                Internal
                    { leftChild = leftChild
                      rightChild = updateRange rightChild low high delta zeroValue add
                      low = l
                      high = h
                      sum = add sum (List.reduce add [ for _ in low..high -> delta ])
                      addend = addend }
            else
                Internal
                    { leftChild = updateRange leftChild low mid delta zeroValue add
                      rightChild = updateRange rightChild (mid + 1) high delta zeroValue add
                      low = l
                      high = h
                      sum = add sum (List.reduce add [ for _ in low..high -> delta ])
                      addend = addend }
