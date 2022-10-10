module SegmentTree : sig
  type t

  val create : int array -> t

  val queryRange : t -> int -> int -> int

  val updateRange : t -> int -> int -> int -> t
end = struct
  type treeNode =
    | Leaf of {value: int}
    | Internal of
        {leftChild: treeNode; rightChild: treeNode; low: int; high: int; sum: int; addend: int}

  type t = treeNode

  let create numbers =
    let rec create' low high =
      if low = high then (Leaf {value= numbers.(low)}, numbers.(low))
      else
        let mid = (low + high) / 2 in
        let leftChild, lsum = create' low mid in
        let rightChild, rsum = create' (mid + 1) high in
        (Internal {leftChild; rightChild; low; high; sum= lsum + rsum; addend= 0}, lsum + rsum)
    in
    let res, _ = create' 0 (Array.length numbers - 1) in
    res

  let queryRange (node : treeNode) (low : int) (high : int) : int =
    let rec queryRange' (node : treeNode) (low : int) (high : int) (accAddend : int) : int =
      match node with
      | Leaf {value} -> accAddend + value
      | Internal {leftChild; rightChild; low= l; high= h; sum; addend} ->
          let mid = (l + h) / 2 in
          if low = l && high = h then sum + ((high - low + 1) * accAddend)
          else if high <= mid then queryRange' leftChild low high (accAddend + addend)
          else if low > mid then queryRange' rightChild low high (accAddend + addend)
          else
            queryRange' leftChild low mid (accAddend + addend)
            + queryRange' rightChild (mid + 1) high (accAddend + addend)
    in
    queryRange' node low high 0

  let rec updateRange node low high delta =
    match node with
    | Leaf {value} -> Leaf {value= value + delta}
    | Internal {leftChild; rightChild; low= l; high= h; sum; addend} ->
        let mid = (l + h) / 2 in
        if low = l && high = h then
          Internal
            { leftChild
            ; rightChild
            ; low= l
            ; high= h
            ; sum= sum + ((high - low + 1) * delta)
            ; addend= addend + delta }
        else if high <= mid then
          Internal
            { leftChild= updateRange leftChild low high delta
            ; rightChild
            ; low= l
            ; high= h
            ; sum= sum + ((high - low + 1) * delta)
            ; addend }
        else if low > mid then
          Internal
            { leftChild
            ; rightChild= updateRange rightChild low high delta
            ; low= l
            ; high= h
            ; sum= sum + ((high - low + 1) * delta)
            ; addend }
        else
          Internal
            { leftChild= updateRange leftChild low mid delta
            ; rightChild= updateRange rightChild (mid + 1) high delta
            ; low= l
            ; high= h
            ; sum= sum + ((high - low + 1) * delta)
            ; addend }
end
