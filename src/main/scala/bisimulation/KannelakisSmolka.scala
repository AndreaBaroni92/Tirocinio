package bisimulation

import lts._

import scala.annotation.tailrec

class KannelakisSmolka(val l: Lts) {

  def split(block: List[Node], a: Label, mapN: Map[String, Int], index: Int)
  : (List[Node], List[Node], Map[String, Int], Int) = {

    def check(s: Node, t: Node) = {

      // l1 e l2 rappresentano gli stati destinazione
      // raggiungibili dai nodi s e t
      val l1 = l.getLabel(s, a)
      val l2 = l.getLabel(t, a)

      // b1 e b2 rappresentano i blocchi a cui appartengono gli stati in l1 e l2
      val b1 = l1.map(x => mapN.getOrElse(x.name, 0)).distinct.sortWith((a, b) => a <= b)
      val b2 = l2.map(x => mapN.getOrElse(x.name, 0)).distinct.sortWith((a, b) => a <= b)

      b1 == b2
    }

    val s = block.head
    val (block1, block2) = block.partition(n => check(s, n))

    def UpdateMap(lis: List[Node], mapN: Map[String, Int], index: Int): Map[String, Int] = {

      val newMap = lis.map(x => (x.name, index)).toMap
      mapN.concat(newMap)

    }

    if (block2.isEmpty) {
      (block1, block2, mapN, index)
    }

    else {
      val newMap = UpdateMap(block2, mapN, index + 1)

      (block1, block2, newMap, index + 1)

    }

  }


  def compute(): List[List[Node]] = {

    //risln viene aggiunto per rendere la funzione tail ricorsiva
    @tailrec
    def singleBloks(risln: List[List[Node]], ln: List[List[Node]], mapN: Map[String, Int], numBlock: Int)
    : (List[List[Node]], Map[String, Int], Int) = {

      // trova un azione che permette di fare lo splitting
      def findSplitterAction(b: List[Node], a: List[String]) = {
        a.find(act => {
          val (_, b2, _, _) = split(b, new Label(act), mapN, numBlock)
          if (b2.isEmpty)
            false
          else
            true
        })
      }

      ln match {
        case ::(head, next) => findSplitterAction(head, l.listLab) match {
          case Some(value) => {
            val (b1, b2, newMap, newNumBlock) = split(head,
              new Label(value),
              mapN,
              numBlock)

            val lst1 = b1 :: b2 :: risln
            (lst1 ::: next, newMap, newNumBlock)

          }
          case None => singleBloks(head :: risln, next, mapN, numBlock)
        }
        case Nil => (risln, mapN, numBlock)

      }


    }

    def callSingleBlocks(ln: List[List[Node]], mapN: Map[String, Int], numBlock: Int)
    : (List[List[Node]], Map[String, Int], Int) = {
      singleBloks(Nil, ln, mapN, numBlock)
    }

    @tailrec
    def iterate(ln: List[List[Node]], mapN: Map[String, Int], numBlock: Int): List[List[Node]] = {

      val (guess, newMapN, newNumBlock) = callSingleBlocks(ln, mapN, numBlock)

      if (guess.length == ln.length)
        guess
      else
        iterate(guess, newMapN, newNumBlock)
    }

    val mapNode = l.nodes.map(x => (x.name, 1)).toMap
    iterate(List(l.nodes), mapNode, 1)

  }

}
