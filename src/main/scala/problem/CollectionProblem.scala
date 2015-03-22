package problem

object CollectionProblem {

  private def choose[A](as : (A)*) = as(r.nextInt(as.size))

  def prompt(op : String, collection: String, _type: String) : String = {
    s"""Write a function that computes the $op of a $collection of ${_type}(s)
       |
       |The function should have the following signature
       |def $op(collection : $collection[${_type}]) : Double""".stripMargin
  }

  def average(numbers : Traversable[Double]) : Double = {
    numbers.sum / numbers.size.toDouble
  }

  def sum(numbers : Traversable[Double]) : Double = numbers.sum

  def stddev(numbers : Traversable[Double]) : Double = {
    val avg = average(numbers)
    Math.sqrt(average(numbers.map(n => (n - avg) * (n - avg))))
  }

  def getNumericType : String = {
    choose(
      "Double",
      "Int"
    )
  }

  def getCollection(typeParam : String) : CollectionDescription = {
    choose(
      CollectionDescription("HashSet", typeParam),
      CollectionDescription("ArrayBuffer", typeParam)
    )
  }

  def getOperation : (Traversable[Double] => Double, String, Seq[String]) = {
    choose(
      (average _, "average", Seq[String]("Use the sum method", "Do this in one statement")),
      (sum _, "sum", "Use the sum method"),
      (stddev _, "standard deviation", "Use the map method")
    )
  }

  def randomInts(n : Int) : Traversable[Int] = Stream.continually(r.nextInt(200)).take(n)

  def randomDoubles(n : Int) : Traversable[Double] = Stream.continually(r.nextDouble() * 200).take(n)

  def showTests(colD : CollectionDescription, doublesToDouble: (Traversable[Double]) => Double, opName : String): String = {
    colD.typeParam match {
      case "Int" => {
        val n = randomInts(5)
        s"For example: $opName($colD(${n mkString ","})) is ${doublesToDouble(n)}"
      }
      case "Double" => {
        val n = randomDoubles(5)
        s"For example: $opName($colD(${n mkString ","})) is ${doublesToDouble(n)}"
      }
    }
  }

  def pickRandom : CollectionProblem = {
    val (operation, opName, req) = getOperation
    val numberType = getNumericType
    val collection : CollectionDescription = getCollection(numberType)
    val madePrompt = makePrompt(
      base = prompt(opName, collection.collection, numberType),
      example = showTests(collection, operation, opName),
      requirements = req
    )
    new CollectionProblem(madePrompt)
  }
}


class CollectionProblem(description : String)
  extends Problem(description)

case class CollectionDescription(collection: String, typeParam: String) {
  override def toString : String = s"$collection[$typeParam]"
}
