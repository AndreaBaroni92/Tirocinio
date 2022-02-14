import scala.annotation.tailrec

val l1 = List((0, 1), (2, 3), (1, 21), (5, 123), (2, 3))
val vec = Vector.fill(9)(0)

l1.foldLeft(vec)((x, y) => x.updated(y._1, y._2))

val vec1 = Vector(7, 2, 13)
val vec2 = Vector(4, 5, 6)
val l1 = List(Vector(1, 2, 3), Vector(1, 2, 3), Vector(1, 2, 12))

@tailrec
def compare_vec(vec1: Vector[Int], vec2: Vector[Int],
                start: Int, end: Int): Boolean = {
  if (start > end)
    true
  else if (vec1(start) < vec2(start))
    true
  else if (vec1(start) > vec2(start))
    false
  else
    compare_vec(vec1, vec2, start + 1, end)
}

class Polynom(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  def +(other: Polynom) =
    new Polynom(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted)
      yield coeff + "x^" + exp) mkString " + "
}

val m1 = Map(1 -> 45.8, 0 -> 5.7) withDefaultValue (0.0)
val f: List[(Int, Int, Int)] = List((-1, 1, 1), (-4, 0, 0))
f.sorted


def fattoriale(n: Int): Int = {
  val ris = if (n == 0)
    1
  else {
    n * fattoriale(n - 1)
  }
  ris
}

class A
class B extends A

val x:A = new B

def foo(x:B):A = {
  x
}
