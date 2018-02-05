package hashcode.qualification

object Knapsack {
  type Value = Long
  type Weight = Int
  //inputs: (value, weight)
  def solve[I](inputs: Iterable[I], valueFun: I ⇒ Value, weightFun: I ⇒ Weight, size: Weight): (Value, Set[I]) = {
    // x x x x x
    // ^---^
    //     y
    def step(prev: List[(Value, Set[I])], item: I) = {
      val (left, right) = prev.splitAt(weightFun(item))
      val value = valueFun(item)
      left ++ (prev,right).zipped.map {
        case ((vl, sl), r @ (vr, sr)) ⇒
          val valWith = vl + value
          if (valWith > vr) (valWith, sl + item) else r
      }
    }
    
    val init = List.fill(size+1)((0L,Set.empty[I]))
    inputs.foldLeft(init)(step).last
  }
}