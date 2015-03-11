package problem


object BasicProblem {
  def pickRandom : BasicProblem[Any, Int] = {
    choose(
      () => new BasicProblem[String, Int]("Write a function that returns the length of a string", a => a.length) {
        override def inputs: Seq[String] = Seq("a", "bb", "")
        override def test(possibleSolution: (String) => Int): Boolean = super.test(possibleSolution)
      },
      () => new BasicProblem[Int, Int]("Write a function that squares an integer", a => a*a) {
        override def inputs: Seq[Int] = Seq(-1,0,1,10,50,37)
        override def test(possibleSolution: (Int) => Int): Boolean = super.test(possibleSolution)
      },
      () => new BasicProblem[Int, Int]("Write a function that subtracts one from an integer", a => a-1) {
        override def inputs: Seq[Int] = Seq(-1,0,1,10,50,37)
        override def test(possibleSolution: (Int) => Int): Boolean = super.test(possibleSolution)
      }
    )
  }
}

abstract class BasicProblem[+A,B](description: String,
                        solution : A => B)
  extends Problem[A,B](description, solution){

}
