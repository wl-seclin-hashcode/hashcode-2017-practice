package hashcode.qualification

import java.io.File
import java.util.Scanner

object Parser {

  def read(f: String): Problem = {

    val scan = new Scanner(new File(f))

    // val nrow = scan.nextInt()
    // val ncol = scan.nextInt()
    // val minIngredients = scan.nextInt()
    // val maxCellsInSlice = scan.nextInt()

    scan.nextLine()

    // val pizza = Vector.fill(nrow) { scan.nextLine() }

    Problem()
  }

}