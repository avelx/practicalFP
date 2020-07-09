object GenericTypes {

  trait Sum[A,B]
  final case class Left[A,B](value:A) extends Sum[A, B]
  final case class Right[A,B](value: B) extends Sum[A, B]

  def intOrString(in: Boolean) : Sum[Int, String] =
    if (in)
      Left[Int, String](1)
    else
      Right[Int, String]("Done")

  sealed trait Maybe[A]
  final case class Full[A](value: A) extends Maybe[A]
  final case class Empty[A]() extends Maybe[A]

  def main(args: Array[String]) : Unit = {
    //println( intOrString(false) )

//    val perhaps : Maybe[Int] = Empty[Int]
//    val perhaps2 : Maybe[Int] = Full(1)

  }
}