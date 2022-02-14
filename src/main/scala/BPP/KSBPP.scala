package BPP

import lts.Partition

import scala.annotation.tailrec

class KSBPP(val net: BPPnet) {


  def split(block: List[Place], indexes: Vector[Int], numBlocks: Int)
  : (List[Place], List[Place], Vector[Int], Int) = {

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

    @tailrec
    def create_placeBlock_vector(from: List[MultiSet], toUpdate: List[Vector[Int]]): List[Vector[Int]] = {

      from match {
        case ::(head, next) =>
          val b1 = Vector.fill(numBlocks + 1)(0) // +1 in quanto il vettore parte da 0
          val ris = head.m.foldLeft(b1)((x, y) =>
            x.updated(indexes(net.indexPlaces getOrElse(y._2.name, -1)), y._1))
          create_placeBlock_vector(next, ris :: toUpdate)
        case Nil => toUpdate.distinct.sortWith((x, y) => compare_vec(x, y, 0, numBlocks + 1))
      }

    }

    def check(b1: List[Vector[Int]], t: Place, action: Int): Boolean = {
      val index_t = net.indexPlaces getOrElse(t.name, -1)
      val l2 = net.vectorTrans(index_t)(action)
      val b2 = create_placeBlock_vector(l2, List[Vector[Int]]())
      b1 equals b2
    }

    @tailrec
    def iter_label(labels: List[Int]): (List[Place], List[Place], Vector[Int], Int) = {

      labels match {
        case ::(head, next) =>
          val index_s = net.indexPlaces getOrElse(block.head.name, -1)
          val l1 = net.vectorTrans(index_s)(head)
          val b1 = create_placeBlock_vector(l1, List[Vector[Int]]())
          val (block_1, block_2) = block.partition(p => check(b1, p, head))

          if (block_2.isEmpty) {
            iter_label(next)
          }
          else {
            val index_new = block_2.foldLeft(indexes)((x, y) => {
              val index_y = net.indexPlaces getOrElse(y.name, -1)
              x.updated(index_y, numBlocks + 1)
            })
            (block_1, block_2, index_new, numBlocks + 1)
          }
        case Nil => (block, List[Place](), indexes, numBlocks)
      }


    }

    iter_label(net.numOfTransition)

  }


  @tailrec
  private def iterOnBlocks(scanned: List[List[Place]],
                           toScan: List[List[Place]],
                           indexes: Vector[Int],
                           numBlocks: Int)
  : (List[List[Place]], Vector[Int], Int, Boolean) = {

    toScan match {
      case ::(head, next) =>
        val ris = split(head, indexes, numBlocks)
        val (block_1, block_2, indexes_new, numBlocks_new) = ris
        if (block_2.isEmpty) {
          iterOnBlocks(head :: scanned, next, indexes, numBlocks)
        }
        else {
          val list_1 = block_1 :: block_2 :: next
          val total_list = list_1 ::: scanned
          (total_list, indexes_new, numBlocks_new, true)
        }
      case Nil => (scanned, indexes, numBlocks, false)
    }

  }

  @tailrec
  private def run(partition: List[List[Place]],
                  indexes:Vector[Int],
                  numBlocks:Int): List[List[Place]] = {

    val res_iter = iterOnBlocks(List[List[Place]](),
      partition,
      indexes,
      numBlocks)

    val (part_new,indexes_new,numBlocks_new,flag) = res_iter

    if (flag)
      run(part_new,indexes_new,numBlocks_new)
    else
      partition
  }

  def compute_team_bis(): List[List[Place]] = {
    run(List(net.s),Vector.fill(net.numPlaces)(1),1)

  }

}

object KSBPP {

  def main(args: Array[String]): Unit = {
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

    val bisim = new KSBPP(bpp)
    println(bisim.compute_team_bis())
  }

}

