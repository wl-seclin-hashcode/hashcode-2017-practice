package hashcode

case class Solution(slices: Seq[Slice])

case class Slice(row1: Int, row2: Int, col1: Int, col2: Int) {
  lazy val cells = for {
    r <- row1 until row2
    c <- col1 until col2
  } yield Point(r, c)

  lazy val size = cells.size

  def intersects(that: Slice) = {
    that.cells.exists(cells.contains)
  }

  def shift(r: Int, c: Int) = Slice(row1 + r, row2 + r, col1 + c, col2 + c)

  override def toString = s"$row1 $col1 ${row2 - 1} ${col2 - 1}"
}

case class Point(r: Int, c: Int)