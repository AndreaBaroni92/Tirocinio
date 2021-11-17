package ValmVector

import scala.annotation.tailrec

class Bisim(b: RefinableStructure,
            s: RefinableStructure,
            o: RefinableStructure,
            tail1: Vector[Int],
            In_transitions: Int => List[Int]) {


  def update1(block1: Int,
              block2: Int,
              blockRef: RefinableStructure,
              split_Ref0: RefinableStructure,
              outs_Ref0: RefinableStructure): (List[Int], List[Int], RefinableStructure, RefinableStructure) = {

    val s = if (blockRef.Size(block1) <= blockRef.Size(block2))
      blockRef.First(block1)
    else
      blockRef.First(block2)

   // println("Size s",blockRef.Size(blockRef.Set(s)) )
    @tailrec
    def iter_s(s1: Int,
               touch_split: List[Int],
               touch_out: List[Int],
               split_Ref1: RefinableStructure,
               outs_Ref1: RefinableStructure,
               countAnd:Int): (List[Int], List[Int], RefinableStructure, RefinableStructure) = {

      if (s1 != 0) {
        //println("s1, ",s1," c= ",countAnd)

        /*
        iter_t scandisce tutti gli elementi all'interno di in_trans
         */
        @tailrec
        def iter_t(in_trans: List[Int],
                   split_Ref2: RefinableStructure,
                   outs_Ref2: RefinableStructure,
                   touch_split2: List[Int],
                   touch_outs2: List[Int]): (RefinableStructure, RefinableStructure, List[Int], List[Int]) = {

          in_trans match {
            case ::(head, next) => {
              //println("head",head)
              val p = split_Ref2.Set(head)
              val o = outs_Ref2.Set(head)
              val touch_split3 = if (split_Ref2.No_marks(p)) p :: touch_split2 else touch_split2
              val touch_outs3 = if (outs_Ref2.No_marks(o)) o :: touch_outs2 else touch_outs2
              val split_Ref3 = split_Ref2.Mark1(head)
              val outs_Ref3 = outs_Ref2.Mark1(head)
              iter_t(next, split_Ref3, outs_Ref3, touch_split3, touch_outs3)

            }

            case Nil => (split_Ref2, outs_Ref2, touch_split2, touch_outs2)
          }

        }

        val (split_Ref4, outs_Ref4, touch_splits3, touch_outs3) = iter_t(In_transitions(s1), split_Ref1, outs_Ref1, touch_split, touch_out)

        iter_s(blockRef.Next(s1), touch_splits3, touch_outs3, split_Ref4, outs_Ref4,countAnd+1)

      }
      else {
        (touch_split, touch_out, split_Ref1, outs_Ref1)
      }

    }

    iter_s(s, Nil, Nil, split_Ref0, outs_Ref0,0)

  }

  @tailrec
  final def updated2(touch_split5: List[Int],
                     unready_Bunches5: List[Int],
                     split_Ref5: RefinableStructure): (List[Int], RefinableStructure) = {
    touch_split5 match {
      case ::(p, next) => {
        val u = if (split_Ref5.Has_many(split_Ref5.Bunch(p))) 0 else split_Ref5.Bunch(p)
        val (p1, split_Ref6) = split_Ref5.Split1(p)


        val unready_Bunches6 = if (u != 0 && p1 != 0) u :: unready_Bunches5 else unready_Bunches5


        updated2(next, unready_Bunches6, split_Ref6)
      }
      case Nil => (unready_Bunches5, split_Ref5)
    }

  }

  @tailrec
  final def updated3(touch_outs5: List[Int],
                     outs_Ref5: RefinableStructure): RefinableStructure = {

    touch_outs5 match {
      case ::(o, next) => {
        val (_, outs_Ref6) = outs_Ref5.Split1(o)
        updated3(next, outs_Ref6)
      }
      case Nil => outs_Ref5
    }

  }

  def updated(block1: Int,
              block2: Int,
              block_Ref: RefinableStructure,
              split_Ref: RefinableStructure,
              outs_Ref: RefinableStructure,
              unready_Bunches: List[Int]): (List[Int], RefinableStructure, RefinableStructure) = {

    val (touch_Split, touch_Outs, split_Ref1, outs_Ref1) = update1(
      block1,
      block2,
      block_Ref,
      split_Ref,
      outs_Ref
    )

    val (unready_Bunches2, split_Ref2) = updated2(touch_Split, unready_Bunches, split_Ref1)

    val outsRef2 = updated3(touch_Outs, outs_Ref1)

    (unready_Bunches2, split_Ref2, outsRef2)

  }


  @tailrec
  final def iterSplitterBunches(start1: Int, //for u := 1 to Splitters.bunches do
                                end1: Int,
                                b1: RefinableStructure,
                                s1: RefinableStructure,
                                o1: RefinableStructure,
                                unready_Bunch: List[Int]
                         ): (RefinableStructure, RefinableStructure, RefinableStructure, List[Int]) = {

    if (start1 <= end1) {
      val t = s1.Bunch_first(start1) //t := Splitters.Bunch_first(u)

      //print("bunch splitter = ",t)
      @tailrec
      def iterT(b2: RefinableStructure,
                tb2: List[Int],
                sp2: RefinableStructure,
                i: Int): (RefinableStructure, List[Int]) = { //while t != 0 do


       // println("i = ",i)
        if (i != 0) {
          val s = tail1(i) //s := tail[t];
          val b = b2.Set(s)
          val tb3 = if (b2.No_marks(b)) b :: tb2 else tb2
          val b3 = b2.Mark1(s)
          iterT(b3, tb3, sp2, sp2.Bunch_next(i))
        }

        else {
          (b2, tb2)
        }

      }

      val (mark1Block, touchedBlocks) = iterT(b1, Nil, s1, t)

      @tailrec
      def iterTouchedBlocks(block_ref7: RefinableStructure,
                            split_ref7: RefinableStructure,
                            outs_ref7: RefinableStructure,
                            unready_bunches7: List[Int],
                            touchBlocks: List[Int]): (RefinableStructure, RefinableStructure, RefinableStructure, List[Int]) = {
        touchBlocks match {
          case ::(head, next) => { //head = b
            val (nuovo, block_ref8) = block_ref7.Split1(head) //nuovo =
           // println("nuovo= ",nuovo)
            if (nuovo != 0) {
              val (unready_bunches8, split_ref8, outs_ref8) = updated(head, nuovo, block_ref8, split_ref7, outs_ref7, unready_bunches7)

              iterTouchedBlocks(block_ref8, split_ref8, outs_ref8, unready_bunches8, next)
            }
            else {
              iterTouchedBlocks(block_ref8, split_ref7, outs_ref7, unready_bunches7, next)
            }

          }
          case Nil => (block_ref7, split_ref7, outs_ref7, unready_bunches7)
        }
      }

      val (bloc_ref9, split_ref9, outs_ref9, unready_bunches9) = iterTouchedBlocks(mark1Block, s1, o1, unready_Bunch, touchedBlocks)


      iterSplitterBunches(start1 + 1, end1, bloc_ref9, split_ref9, outs_ref9, unready_bunches9)
    }
    else {
      (b1, s1, o1, unready_Bunch)

    }

  }




  @tailrec
  final def iterUnreadyBunches(
                                block_ref10: RefinableStructure,
                                split_ref10: RefinableStructure,
                                outs_ref10: RefinableStructure,
                                unready_bunch10: List[Int]
                              ): RefinableStructure = {

    unready_bunch10 match {
      case ::(u, next) => {
        val (p, split_ref11) = split_ref10.Extract_set(u)
        val unready_bunch11 = if (split_ref11.Has_many(u)) unready_bunch10 else next
        val t = split_ref11.First(p)

        @tailrec
        def iter_t(i: Int,
                   block_ref11: RefinableStructure,
                   touch_b11: List[Int],
                  ): (RefinableStructure, List[Int]) = {
          if (i != 0) {
            if (i == outs_ref10.First(outs_ref10.Set(i))) {
              val s = tail1(i)
              val set_Of_s = block_ref11.Set(s)
              val touch_b12 = if (block_ref11.No_marks(set_Of_s)) set_Of_s :: touch_b11 else touch_b11
              val i1 = outs_ref10.Left_neighbour(i)
              val i2 = outs_ref10.Right_neighbour(i)
              if ((i1 > 0 && tail1(i1) == s && split_ref11.Bunch((split_ref11.Set(i1))) == u)
                || (i2 > 0 && tail1(i2) == s && split_ref11.Bunch((split_ref11.Set(i2))) == u)) {
               // println("Mark1s", s)
                val block_ref12 = block_ref11.Mark1(s)
                iter_t(split_ref11.Next(i), block_ref12, touch_b12)
              }
              else {
                //println("Mark2", s)
                val block_ref12 = block_ref11.Mark2(s)
                iter_t(split_ref11.Next(i), block_ref12, touch_b12)
              }

            }
            else {
              iter_t(split_ref11.Next(i), block_ref11, touch_b11)
            }

          }
          else {
            (block_ref11, touch_b11)
          }
        }

        val (block_ref13, touch_b13) = iter_t(t, block_ref10, Nil)

        @tailrec
        def iter_touched_blocks(block_ref14: RefinableStructure,
                                split_ref14: RefinableStructure,
                                outs_ref14: RefinableStructure,
                                unready_bunch14: List[Int],
                                touch_b14: List[Int]): (RefinableStructure, RefinableStructure, RefinableStructure, List[Int]) =

          touch_b14 match {
            case ::(set_t1, next) => {
              val (set_t2, block_ref15) = block_ref14.Split1(set_t1)
              if (set_t2 != 0) {
                val (unready_bunch15, split_ref15, outs_ref15) = updated(set_t1,
                  set_t2,
                  block_ref15,
                  split_ref14,
                  outs_ref14,
                  unready_bunch14
                )

                val (set_t3, block_ref16) = block_ref15.Split2(set_t1)
                if (set_t3 != 0) {
                  val (unready_bunch16, split_ref16, outs_ref16) = updated(set_t1,
                    set_t3,
                    block_ref16,
                    split_ref15,
                    outs_ref15,
                    unready_bunch15
                  )

                  iter_touched_blocks(block_ref16, split_ref16, outs_ref16, unready_bunch16, next)
                }
                else {
                  iter_touched_blocks(block_ref16, split_ref15, outs_ref15, unready_bunch15, next)
                }


              }
              else {
                val (set_t3, block_ref16) = block_ref15.Split2(set_t1)
                if (set_t3 != 0) {
                  val (unready_bunch16, split_ref16, outs_ref16) = updated(set_t1,
                    set_t3,
                    block_ref16,
                    split_ref14,
                    outs_ref14,
                    unready_bunch14
                  )

                  iter_touched_blocks(block_ref16, split_ref16, outs_ref16, unready_bunch16, next)
                }
                else {
                  iter_touched_blocks(block_ref16, split_ref14, outs_ref14, unready_bunch14, next)
                }
              }
            }
            case Nil => (block_ref14, split_ref14, outs_ref14, unready_bunch14)
          }


        val (block_ref18, split_ref18, outs_ref18, unready_bunch18) = iter_touched_blocks(block_ref13, split_ref11, outs_ref10, unready_bunch11, touch_b13)
        iterUnreadyBunches(block_ref18, split_ref18, outs_ref18, unready_bunch18)
      }
      case Nil => block_ref10
    }

  }



  def computeBisim(): RefinableStructure = {


    val (block_ref,split_ref,outs_ref,unready_bunch): (RefinableStructure,
      RefinableStructure,
      RefinableStructure,
      List[Int]) = iterSplitterBunches(1,
      s.bunches,
      b,
      s,
      o,
      Nil)

    //println("Prima Fase fatta")
    val ris: RefinableStructure = iterUnreadyBunches(block_ref, split_ref, outs_ref, unready_bunch)
    ris

  }

}
