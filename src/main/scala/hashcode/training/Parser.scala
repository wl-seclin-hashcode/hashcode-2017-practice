package hashcode.training

import java.io.File
import java.util.Scanner

object Parser {
  def read(f: String): Problem = {
    val scan = new Scanner(new File(f))
    import scan._
    val nrow = nextInt()
    val ncol = nextInt()
    val minIngredients = nextInt()
    val maxCellsInSlice = nextInt()

    nextLine()
    val pizza = Vector.fill(nrow) { nextLine() }

    Problem(pizza, nrow, ncol, minIngredients, maxCellsInSlice, name = f)
  }
}