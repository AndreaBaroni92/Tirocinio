package bisimulation

import lts._

import scala.annotation.tailrec


class FixedPoint(val l: Lts) {

  def fixedPointBisim(): List[(Node, Node)] = {

    val start = l.nodes.flatMap(x => l.nodes.map(y => (x, y))) // prodotto cartesiano stati Q X Q


    def check(p: Node, q: Node, rel: List[(Node, Node)])
    : Boolean = {

      l.mapNode.get(p.name) match {
        case Some(value) => value.forall(x => {
          l.mapNode.get(q.name) match {
            case Some(value1) => value1.exists(y => {
              x._2.name == y._2.name &&
                rel.exists(s => {
                  s._1.name == x._3.name && s._2.name == y._3.name
                })
            })
            case None => false //p muove ma q non muove
          }
        })
        case None => true // p non muove
      }
    }

    @tailrec
    def iterate(rel: List[(Node, Node)]): List[(Node, Node)] = {

      val guess = rel.filter(x => {
        check(x._1, x._2, rel) &&
          check(x._2, x._1, rel)
      })

      if (guess.size == rel.size)
        rel
      else {
        //println(guess)
        iterate(guess)
      }

    }

    iterate(start)

  }

}
