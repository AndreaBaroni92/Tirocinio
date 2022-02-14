package BPP

import scala.annotation.tailrec


class FixedPointBPP(val net: BPPnet) {

  val start: List[(Place, Place)] = for {
    x <- net.s
    y <- net.s
  } yield (x, y)

  val matrix: Vector[Vector[Boolean]] = Vector.fill(net.numPlaces, net.numPlaces)(true)

  def checkAddClosure(m1: MultiSet, m2: MultiSet, rel: Vector[Vector[Boolean]]): Boolean = {

    @tailrec
    def checkSingleItem(index: Int, toSearch: List[(Int, Place)], updated: List[(Int, Place)])
    : Option[List[(Int, Place)]] = {

      toSearch match {
        case ::(head, next) =>
          val head_id = net.indexPlaces.getOrElse(head._2.name, -1)

          if (rel(index)(head_id) && (head._1 != 0)) {
            val head_new = (head._1 - 1, head._2)
            val ris = head_new :: next
            Some(ris ::: updated)
          }
          else {
            checkSingleItem(index, next, head :: updated)
          }
        case Nil => None
      }

    }

    @tailrec
    def iterNumItem(num: Int, index: Int, updated: List[(Int, Place)])
    : Option[List[(Int, Place)]] = {

      if (num == 0) {
        Some(updated)
      }
      else {

        val ris = checkSingleItem(index, updated, Nil)

        ris match {
          case Some(value) => iterNumItem(num - 1, index, value)
          case None => None
        }
      }
    }

    @tailrec
    def iterMultiSet(toIter: List[(Int, Place)], toUpdate: List[(Int, Place)]): Boolean = {
      toIter match {
        case ::(head, next) =>
          val head_id = net.indexPlaces getOrElse(head._2.name, -1)
          val ris = iterNumItem(head._1, head_id, toUpdate)
          ris match {
            case Some(value) => iterMultiSet(next, value)
            case None => false
          }
        case Nil => true
      }
    }

    val l1:Int = m1.m.foldLeft(0)((x,y)=> x + y._1)
    val l2:Int = m2.m.foldLeft(0)((x,y)=> x + y._1)

    if (l1 != l2)
      false
    else
      iterMultiSet(m1.m, m2.m)

  }

  def check(s1: Place, s2: Place, matrix: Vector[Vector[Boolean]])
  : Boolean = {

    def checkAction(action: Int): Boolean = {

      val v1 = net.vectorTrans(net.indexPlaces getOrElse(s1.name, -1))(action)
      val v2 = net.vectorTrans(net.indexPlaces getOrElse(s2.name, -1))(action)

      v1.forall(x => v2.exists(y => checkAddClosure(x, y, matrix)))
    }

    net.numOfTransition.forall(x => checkAction(x))
  }

  @tailrec
  private def updateMatrix(matrix: Vector[Vector[Boolean]],
                           toMark: List[(Place, Place)]): Vector[Vector[Boolean]] = {

    toMark match {
      case ::(head, next) =>
        val i = net.indexPlaces getOrElse(head._1.name, -1)
        val j = net.indexPlaces getOrElse(head._2.name, -1)
        val newLines = matrix(i).updated(j, false)
        updateMatrix(matrix.updated(i, newLines), next)

      case Nil => matrix
    }

  }


  @tailrec
  private def iterate(rel: List[(Place, Place)],
                      matr: Vector[Vector[Boolean]]): (List[(Place, Place)],Vector[Vector[Boolean]])
  = {

    val (guess1, toBeMarked) = rel.partition(x => {
      check(x._1, x._2, matr) &&
        check(x._2, x._1, matr)
    })

    if (toBeMarked.isEmpty)
      (rel,matr)
    else {
      val new_matrix = updateMatrix(matr, toBeMarked)
      iterate(guess1, new_matrix)
    }


  }

  def computeTeamBisim():(List[(Place, Place)],Vector[Vector[Boolean]]) = {
    iterate(start, matrix)
  }


}

object FixedPointBPP {
  def main(args: Array[String]): Unit = {
    /*
    val s4 = new Place("s4")
    val s5 = new Place("s5")
    val s6 = new Place("s6")
    val s7 = new Place("s7")
    val s8 = new Place("s8")
    val s9 = new Place("s9")

    val a = new Label("a")
    val b = new Label("b")
    val c = new Label("c")

    val t1 = new Transition(s4, a, new MultiSet(List((1, s5), (1, s6))))

    val t2 = new Transition(s4, a, new MultiSet(List((2, s7))))
    val t3 = new Transition(s8, c, new MultiSet(List((1, s4))))
    val t4 = new Transition(s9, c, new MultiSet(List((1, s4))))
    val t5 = new Transition(s5, b, new MultiSet(List((1, s8))))
    val t6 = new Transition(s6, b, new MultiSet(List((1, s9))))
    val t7 = new Transition(s7, b, new MultiSet(List((1, s9))))

    val bpp = new BPPnet(List(s4, s5, s6, s7, s8, s9),
      List(t1, t2, t3, t4, t5, t6, t7))


    val bisim = new FixedPointBPP(bpp)

    println(bisim.computeTeamBisim())


     */

    val s1 = new Place("s1")
    val s2 = new Place("s2")
    val s3 = new Place("s3")
    val s4 = new Place("s4")
    val s5 = new Place("s5")
    val s6 = new Place("s6")

    val inc = new Label("inc")
    val dec = new Label("dec")

    val t1 = new Transition(s1,inc,new MultiSet(List((1,s1),(1,s2))))
    val t2 = new Transition(s2,dec,new MultiSet(Nil))

    val t3 = new Transition(s3,inc,new MultiSet(List((1,s4),(1,s5))))
    val t4 = new Transition(s4,inc,new MultiSet(List((1,s6),(1,s3))))
    val t5 = new Transition(s6,dec,new MultiSet(Nil))
    val t6 = new Transition(s5,dec,new MultiSet(Nil))

    val bpp = new BPPnet(List(s1,s2,s3,s4,s5,s6),
      List(t1,t2,t3,t4,t5,t6))

    val bisim = new FixedPointBPP(bpp)

    val mark1 = new MultiSet(List((3,s1),(4,s2)))
    val mark2 = new MultiSet(List((3,s3),(3,s6),(1,s5)))

    val (rel,matr) = bisim.computeTeamBisim()


    println(rel)

    println(bisim.checkAddClosure(mark1,mark2,matr))

  }
}
