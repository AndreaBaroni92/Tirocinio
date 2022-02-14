package bisimulation
// versione migliorata dell'algoritmo di ks in cui utilizzo un vettore di due dimensioni
// per controllare le transizioni di uno stato e di una label
// inoltre in questa versione itero esplicitamente sulle Label


import lts.{Label, Lts, Node}
import util.processInp

import scala.annotation.tailrec

class KS5(val l: Lts) {

  val tr: Vector[Vector[Vector[(Int, Int, Int)]]] = l.vectorTrans

  def split(ln: List[Node],
            blockOfNode: Vector[Int],
            numBlocks: Int
           ): (List[Node], List[Node], Vector[Int], Int) = {

    def check(vec1:Set[Int],t: Node, action: Int): Boolean = {
      val numt = l.indexNodes.getOrElse(t.name, -1)
      val vec2 = tr(numt)(action).map(x => blockOfNode(x._3))
      vec1 == vec2.toSet
    }

    @tailrec
    def iterAction(actions: List[Int])
    : (List[Node], List[Node], Vector[Int], Int) = {
      actions match {
        case ::(cons_a, tail_a) =>
          val s = ln.head
          val nums = l.indexNodes.getOrElse(s.name, -1)
          val vec1 = tr(nums)(cons_a)
            .map(x => blockOfNode(x._3))
            .toSet
          val (b1, b2) = ln.partition(t => check(vec1, t, cons_a))
          if (b2.isEmpty) {
            iterAction(tail_a)
          }

          //e'successo uno split con l'azione cons_a

          else {
            @tailrec
            def UpdateVector(list: List[Node],
                             vec: Vector[Int], index: Int)
            : Vector[Int] = {
              list match {
                case ::(head, next) =>
                  UpdateVector(next,
                    vec.updated(
                      l.indexNodes.getOrElse(head.name, -1),
                      index
                    ),
                    index)
                case Nil => vec
              }
            }

            val new_blockOfNode = UpdateVector(b2,
              blockOfNode, numBlocks + 1)
            (b1, b2, new_blockOfNode, numBlocks + 1)
          }
        case Nil => (ln, Nil, blockOfNode, numBlocks)
      }
    }

    iterAction(l.numIndex)
  }

  @tailrec
  final def iterateBlock(scanned: List[List[Node]],
                         toScan: List[List[Node]],
                         blockOfNode: Vector[Int],
                         numBlocks: Int
                        )
  : (List[List[Node]], Vector[Int], Int, Boolean) = {


    toScan match {
      case ::(head, next) =>
        val ris = split(head, blockOfNode, numBlocks)

        val (b1, b2, new_blockOfNode, new_numBlocks) = ris

        if (b2.isEmpty) {

          iterateBlock(head :: scanned, next, blockOfNode, numBlocks)
        }
        else {

          val two_blocks = b1 :: b2 :: next

          (two_blocks ::: scanned, new_blockOfNode, new_numBlocks, true)
        }
      case Nil => (scanned, blockOfNode, numBlocks, false)
    }
  }


  @tailrec
  final def iter(part: List[List[Node]],
                 blockOfNode: Vector[Int],
                 numBlocks: Int
                ): List[List[Node]] = {

    val ris_itB = iterateBlock(Nil, part, blockOfNode, numBlocks)
    val (new_part, new_blockOfNode, new_numBlocks, flag) = ris_itB

    if (flag) {

      iter(new_part, new_blockOfNode, new_numBlocks)
    }
    else
      part

  }


}

object KS5 {

  def compBisim(path: String): List[List[Node]] = {
    val myLts = processInp.getLts(path)
    new KS5(myLts).iter(List(myLts.nodes), Vector.fill(myLts.numNodes)(1), 1)
  }

  def main(args: Array[String]): Unit = {
    val ris = compBisim("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\inp4")

    println(ris)


  }
}

