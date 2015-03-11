import scala.util.Random

/**
 * Created by Gustavo on 3/10/15.
 */
package object problem {

  val r = new Random()

  /**
   * Ideally this would have read
   * choose(as : ( => A)*) but the compiler does not like variable arguments
   * of by name parameters!
   * @param as Functions that give As to choose from
   * @tparam A any type
   * @return A choice of an A from the inputs
   */
  def choose[A](as : (() => A)*) : A = as(r.nextInt(as.size))()
}
