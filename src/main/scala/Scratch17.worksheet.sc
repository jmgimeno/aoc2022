def toBinStr(b: Byte) =
  (List.fill(7)("0") ++
    List
      .unfold(b) { b =>
        if b != 0
        then Some((b % 2).toString, (b / 2).toByte)
        else None
      }
      .reverse).foldLeft("")(_ + _).takeRight(7)

toBinStr(0)
toBinStr(1)
toBinStr(2)
toBinStr(3)
toBinStr(4)
