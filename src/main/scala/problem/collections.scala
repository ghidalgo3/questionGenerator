package problem

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


//i need to generate 4 elements of a question:

//description : Human readable description of what to do
//solution : Generated alongside the description
//test cases : generated alongside the description
//requirement validation: not sure how to do this yet


object CollectionProblem {


  private def choose[A](as : (A)*) = as(r.nextInt(as.size))

  val prompt : (String, String, String) => String = {
    (result, col, typ) => s"Compute the $result given a $col of $typ"
  }

  def average(numbers : Traversable[Double]) : Double = {
    numbers.sum / numbers.size.toDouble
  }

  def sum(numbers : Traversable[Double]) : Double = numbers.foldLeft(0.0)(_ + _)

  def getCollection : (Traversable[Double], String) = {
    choose(
      (new mutable.HashSet(), "Set"),
      (new ArrayBuffer(), "Array")
    )
  }

  def getOperation : (Traversable[Double] => Double, String) = {
    choose(
      (average(_), "average"),
      (sum(_), "sum")
    )
  }

//  def generateTestCase(function : Traversable[Double] => Double) : (Traversable[Double] => Double) => Boolean = {
//    val ns = randomDoubles(20)
//    theirFunction => theirFunction(ns) == function(ns)
//  }

  def randomDoubles(n : Int) : Traversable[Double] = {
    val result = new Array[Double](n)
    for(i <- 0 until n) result(i) = r.nextInt(200)
    result
  }

  def pickRandom : CollectionProblem = {
    val (operation, opName) = getOperation
    val (collection, colName)= getCollection
    new CollectionProblem(prompt(opName, colName, "Double"), operation) {
      override def inputs: Seq[Traversable[Double]] = {
        Seq(randomDoubles(10), randomDoubles(20), randomDoubles(50))
      }
    }
  }
}

/**
 * Represents questions about collections classes with doubles
 *
 * @param description
 * @param solution
 */
abstract class CollectionProblem(description : String,
                                 solution : Traversable[Double] => Double)
  extends Problem[Traversable[Double], Double](description, solution) {

}
