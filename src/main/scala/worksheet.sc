import scala.util.Random

//first let me think of a working example involving math problems
//so it looks like recursive generation is not too trick
//but constraint satisfaction is trickier, and it would be "better"
//to generate expressions that satisfy constraints deterministically

//yay constraint satisfaction!

sealed trait Expression {
  def evaluate : Option[Int]
  def toString : String
}
case class Sum(a : Expression, b : Expression) extends Expression {
  override def evaluate: Option[Int] = {
    for(v <- a.evaluate;
        u <- b.evaluate) yield u + v
  }
  override def toString : String = s"($a) + ($b)"
}
case class Product(a : Expression, b : Expression) extends Expression {
  override def evaluate: Option[Int] =  {
    for(v <- a.evaluate;
        u <- b.evaluate) yield u * v
  }
  override def toString : String = s"($a) * ($b)"
}
case class Term(term : Int) extends Expression {
  override def evaluate: Option[Int] = Some(term)
  override def toString : String = s"$term"
}
case class Unknown(name : String) extends Expression{
  override def evaluate: Option[Int] = None
  override def toString : String = name
}
implicit val Int2Term : Int => Term = Term(_)
implicit val String2Unknown : String => Unknown = Unknown(_)
val rand : Random = new Random(10)

def generate(depth : Int)(implicit cons: Constraint) : Expression = {
  depth match {
    case 0 => rand.nextInt(10)
    case _ => rand.nextBoolean() match {
      case true => {
        Sum(generate(depth - 1)(cons), generate(depth - 1)(cons))
      }
      case _ =>  {
        var possibility = Product(generate(depth - 1)(cons), generate(depth - 1)(cons))
        while(!cons.satisfies(possibility)) {
          possibility = Product(generate(depth - 1)(cons), generate(depth - 1)(cons))
        }
        possibility
      }
    }
  }
}

sealed trait Constraint {
  def satisfies(e : Expression) : Boolean
}

implicit val multiplyByZero : Constraint = new Constraint {
  override def satisfies(e: Expression): Boolean = {
    e match {
      case Product(u : Term,v : Term) => !(u.evaluate.get == 0 || v.evaluate.get == 0)
      case Product(u : Expression,v : Expression) => satisfies(u) && satisfies(v)
      case Sum(u,v) => satisfies(u) && satisfies(v)
      case _ => true
    }
  }
}

multiplyByZero.satisfies(Product(0,1))

val gh = generate(5)
multiplyByZero.satisfies(gh)
