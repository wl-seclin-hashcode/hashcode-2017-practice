package hashcode

case class Problem(pizza: Vector[String], nrow: Int, ncol: Int, minIngredients: Int, maxCells: Int) {
  def ingredientAt(r: Int, c: Int): Char = pizza(r)(c)
}
