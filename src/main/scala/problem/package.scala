import scala.util.Random

/**
 * Created by Gustavo on 3/10/15.
 */
package object problem {

  val r = new Random()

  implicit def any2lazyFunction : Any => () => Any = a => () => a
  implicit def intTraversable2DoubleTraversable : Traversable[Int] => Traversable[Double] = {
    original => original.map(_.toDouble)
  }
  implicit def string2StringSequence : String => Seq[String] = s => Seq[String](s)

  def makePrompt(base : String, requirements : Seq[String] = Seq[String](""), example : String = "") : String = {
    s"""$base
       |
       |Requirements:
       |${requirements mkString "\n"}
       |
       |$example
     """.stripMargin
  }

  def pickRandom : Problem = {
    choose(
      CollectionProblem.pickRandom,
      BasicProblem.pickRandom
    )
  }

  /**
   * Ideally this would have read
   * choose(as : ( => A)*) but the compiler does not like variable arguments
   * of by name parameters!
   * @param as Functions that give As to choose from
   * @tparam A any type
   * @return A choice of an A from the inputs
   */
  def choose[A](as : A*) : A = as(r.nextInt(as.size))
}
