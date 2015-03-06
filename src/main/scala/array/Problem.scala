package array

import scala.reflect.ClassTag

/**
 * Most array problems involve iterating through the array and determining something
 * about it. Said differently, most array problems are for() loops.
 */
object Problem {

  type ArrayMapping[A,B] = Array[A] => Array[B]
  type ArrayReduction[A,B] = Array[A] => B

  //input constraints are domain restrictions
  //output constraints are co-domain restrictions
  def generateMapping[A,B](inputConstraints : Seq[Constraint[A]],
                    outputConstraints : Seq[Constraint[B]])(op : A => B)(implicit c:ClassTag[B]) : (String,ArrayMapping[A,B]) = {
    val description = new StringBuilder
    description.append(s"Write a function that accepts an Array[] and returns an Array[]\n")
    description.append(s"It must meet these specifications\n")
    description.append(s"Inputs:\n")
    inputConstraints.foreach(c => description.append(s"    -$c\n"))
    description.append(s"Outputs:\n")
    outputConstraints.foreach(c => description.append(s"    -$c\n"))
    val f : ArrayMapping[A,B] = arr => {
      val validAs = for(cons <- inputConstraints ;
                        a <- arr if cons.test(a)) yield { a }
      val mappedBs = validAs.map(op)
      val validBs = for(cons <- outputConstraints;
                        b <- mappedBs if cons.test(b)) yield { b }
      validBs.toArray
    }
    (description.toString(), f)
  }

  def generateReduction[A,B](inputConstraints : Seq[Constraint[A]],
                             outputConstraints : Seq[Constraint[B]])()(z: B)(op : (B,A) => B)(implicit c:ClassTag[B]) : (ArrayReduction[A,B]) = {
    arr : Array[A] => {
      arr.foldLeft(z)(op)
    }
  }
}

class  Problem
