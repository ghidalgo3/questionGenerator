package problem


object BasicProblem {
  def pickRandom : BasicProblem = {
    choose(
      new BasicProblem("Write a function that returns the length of a string"),
      new BasicProblem("Write a function that squares an integer"),
      new BasicProblem("Write a function that subtracts one from an integer")
    )
  }
}

class BasicProblem(description: String)
  extends Problem(description)
