
import upickle.default._

case class Person(name: String, age: Int)
object Person{
  implicit def rw: ReadWriter[Person] = macroRW
}


write(List(1, 2, 3, 4))

write(Person("as",10))