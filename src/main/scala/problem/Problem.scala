package problem

/**
 * A Problem[A,B] represents a question whose answer is written in terms
 * of a function from A => B.
 */
abstract class Problem[+A,-B](description : String, solution : A => B) {

  /**
   * Validates against the test cases
   * @param possibleSolution a possible solution
   * @return true if passes all test cases, false otherwise
   */
  def test(possibleSolution : A => B): Boolean = {
    inputs.forall{
      a => possibleSolution(a) == solution(a)
    }
  }

  /**
   * Generate a series of inputs to test against
   * @return
   */
  def inputs : Seq[A]

  override def toString = description
}
