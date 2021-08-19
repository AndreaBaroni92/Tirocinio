package bisimulation

import lts._

import scala.annotation.tailrec

class KannelakisSmolka(val l: Lts) {

  def split(block: List[Node], a: Label, pa: Partition) = {


    val paIndex = pa.p.zipWithIndex

    def getBlock(n: Node) = {
      // head in quanto uno stato puo' appartenere a un solo blocco
      paIndex.filter(l => l._1.exists(y => y.name == n.name)).map(x => x._2).head
    }


    def check(s: Node, t: Node) = {

      // l1 e l2 rappresentano gli stati destinazione
      // raggiungibili dai nodi s e t
      val l1 = l.getLabel(s, a)
      val l2 = l.getLabel(t, a)

      // b1 e b2 rappresentano i blocchi a cui appartengono gli stati in l1 e l2
      val b1 = l1.map(x => getBlock(x)).distinct.sortWith((a, b) => a <= b)
      val b2 = l2.map(x => getBlock(x)).distinct.sortWith((a, b) => a <= b)

      // viene controllato che le liste b1 e b2 siano uguali
      if (b1.length != b2.length)
        false
      else
        b1.forall(el1 => b2.exists(el2 => el1 == el2))

    }

    block.partition(n => check(block.head, n))
  }


  def compute() = {
    //risln viene aggiunto per rendere la funzione tail ricorsiva
    @tailrec
    def singleBloks(risln: List[List[Node]], ln: List[List[Node]]): List[List[Node]] = {

      // trova un azione che permette di fare lo splitting
      def findSplitterAction(b: List[Node], a: List[String]) = {
        a.find(act => {
          val (_, b2) = split(b, new Label(act), new Partition(risln ::: ln))
          if (b2.isEmpty)
            false
          else
            true
        })
      }

      ln match {
        case ::(head, next) => findSplitterAction(head, l.listLab) match {
          case Some(value) => {
            val (b1, b2) = split(head, new Label(value), new Partition(risln ::: ln))

            //singleBloks(twoBlocks._1::twoBlocks._2 ::risln ,next)
            val lst1 = b1 :: b2 :: risln
            lst1 ::: next

          }
          case None => singleBloks(head :: risln, next)
        }
        case Nil => risln

      }


    }

    def callSingleBlocks(ln: List[List[Node]]): List[List[Node]] = {
      singleBloks(Nil, ln)
    }

    @tailrec
    def iterate(ln: List[List[Node]]): List[List[Node]] = {

      val guess = callSingleBlocks(ln)

      if (guess.length == ln.length)
        guess
      else
        iterate(guess)
    }

    iterate(List(l.nodes))


  }

}
