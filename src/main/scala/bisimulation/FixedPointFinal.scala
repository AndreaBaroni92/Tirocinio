package bisimulation

import lts._
import util.processInp

import scala.annotation.tailrec


class FixedPointFinal(val l: Lts) {

  def fixedPointBisim(): List[(Node, Node)] = {

    val start = l.nodes.flatMap(x => l.nodes.map(y => (x, y)))
    val matrix= (0 until l.numNodes)
      .map(_ => true)
      .toVector
      .map(_ => (0 until l.numNodes)
        .map(_ => true).toVector)

    def check(p: Node, q: Node,
              matrix: Vector[Vector[Boolean]])
    : Boolean = {

      def checkAction(action:Int) ={
        val v1 = l.vectorTrans(l.indexNodes getOrElse(p.name,-1))(action)
        val v2 = l.vectorTrans(l.indexNodes getOrElse(q.name,-1))(action)
        v1.forall(x => v2.exists(y => matrix(x._3)(y._3)) )
      }
      l.numIndex.forall(x => checkAction(x))
    }

    @tailrec
    def updateMatrix(matrix: Vector[Vector[Boolean]],
                     toMark:List[(Node,Node)]):Vector[Vector[Boolean]] = {

      toMark match {
        case ::(head, next) =>
          val i = l.indexNodes getOrElse(head._1.name, -1)
          val j = l.indexNodes getOrElse(head._2.name, -1)
          val newLines = matrix(i).updated(j,false)
          updateMatrix(matrix.updated(i,newLines),next)
        case Nil => matrix
      }

    }
    @tailrec
    def iterate(rel: List[(Node, Node)],
                matr:Vector[Vector[Boolean]]):List[(Node, Node)]
    = {
      val (guess1,toBeMarked) = rel.partition(x => {
        check(x._1, x._2, matr) &&
          check(x._2, x._1, matr)
      })
      if (toBeMarked.isEmpty)
        rel
      else {
        val newMatrix= updateMatrix(matr,toBeMarked)
        iterate(guess1,newMatrix)
      }

    }
    iterate(start,matrix)
  }
}

object FixedPointFinal {
  def main(args: Array[String]): Unit = {
    val myLts = processInp.getLts("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\inp4")
    val t0 = System.nanoTime()
    val alg = new FixedPointFinal(myLts)


    println(alg.fixedPointBisim())
    val t1 = System.nanoTime()

    println(s"Elapsed time ${(t1-t0)/ 1000000}")

  }
}
