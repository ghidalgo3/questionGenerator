package array

/**
 * Created by Gustavo on 3/6/15.
 */
case class Constraint[A](predicate : A => Boolean, description: String) {

  def test = predicate
  override def toString = description
}
