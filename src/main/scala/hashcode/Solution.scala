package hashcode

case class Solution(slices: Seq[Slice])

case class Slice(row1: Int, row2: Int, col1: Int, col2: Int) {
  def cells = for {
    r <- (row1 min row2) until (row1 max row2)
    c <- (col1 min col2) until (col1 max col2)
  } yield Point(r, c)

  override def toString = s"$row1 $col1 ${row2 - 1} ${col2 - 1}"
}

case class Point(r: Int, c: Int)