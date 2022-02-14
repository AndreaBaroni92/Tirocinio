package ValmVector

import scala.annotation.tailrec

class RefinableStructure(val elems: Vector[Int],
                         val loc: Vector[Int],
                         val first: Vector[Int],
                         val mid1: Vector[Int],
                         val mid2: Vector[Int],
                         val end: Vector[Int],
                         val ufirst: Vector[Int],
                         val uend: Vector[Int],
                         val sidx: Vector[Int],
                         val uidx: Vector[Int],
                         val sets: Int,
                         val bunches: Int,
                         val sizeElems:Int) {

  //val items = elems.max

  //modifica 20 11
  //val items = elems.length - 1

  val items = sizeElems

  def Size(s: Int): Int = end(s) - first(s)

  def Set(e: Int): Int = sidx(e)

  def First(s: Int): Int = elems(first(s))

  def Next(e: Int): Int = {
    if ( loc(e) + 1 >= end(sidx(e)))
      0
    else
      elems(loc(e) + 1)

  }

  def Mark1(e: Int): RefinableStructure = {
    val s = sidx(e)
    val l = loc(e)
    val m = mid1(s)


    if (l >= m && l < mid2(s)) {
      val mid1up1 = mid1.updated(s, m + 1)
      val elemsup1 = elems.updated(l, elems(m))
      val locup1 = loc.updated(elemsup1(l), l)
      val elemsup2 = elemsup1.updated(m, e)
      val locup2 = locup1.updated(e, m)
      new RefinableStructure(elemsup2,
        locup2,
        first,
        mid1up1,
        mid2,
        end,
        ufirst,
        uend,
        sidx,
        uidx,
        sets,
        bunches,
        items)
    }

    else {
      this
    }

  }

  def Mark2(e: Int): RefinableStructure = {
    val s = sidx(e)
    val l = loc(e)
    val m = mid2(s) - 1


    if (l >= mid1(s) && l <= m) {

      val mid2up1 = mid2.updated(s, m)
      val elemsup1 = elems.updated(l, elems(m))
      val locup1 = loc.updated(elemsup1(l), l)
      val elemsup2 = elemsup1.updated(m, e)
      val locup2 = locup1.updated(e, m)

      new RefinableStructure(elemsup2,
        locup2,
        first,
        mid1,
        mid2up1,
        end,
        ufirst,
        uend,
        sidx,
        uidx,
        sets,
        bunches,
        items)
    }
    else {
      this
    }

  }


  @tailrec
  final def iter(start: Int,
                 end: Int,
                 setsp: Int,
                 sidxp: Vector[Int],
                 elemsp: Vector[Int]): Vector[Int] = {

    if (start > end) {
      sidxp
    } else {
      iter(start + 1,
        end,
        setsp,
        sidxp.updated(elemsp(start), setsp),
        elemsp)
    }


  }


  def Split1(s: Int): (Int, RefinableStructure) = {

    if (mid1(s) == mid2(s)) {
      val mid1up = mid1.updated(s, first(s))
      val nuovaRef = new RefinableStructure(elems,
        loc,
        first,
        mid1up,
        mid2,
        end,
        ufirst,
        uend,
        sidx,
        uidx,
        sets,
        bunches,
        items)
      (0, nuovaRef)
    }
    else if (mid1(s) == first(s)) {
      (0, this)
    }

    else {

      val setsup1 = sets + 1
      val uidxup1 = uidx :+ uidx(s)
      val firstup1 = first :+ first(s)
      val endup1 = end :+ mid1(s)
      val firstup2 = firstup1.updated(s, mid1(s))
      val mid1up1 = mid1 :+ firstup2(setsup1)
      val mid2up1 = mid2 :+ endup1(setsup1)

      val sidxup1 = iter(firstup1(setsup1),
        endup1(setsup1) - 1,
        setsup1,
        sidx,
        elems
      )
      (setsup1, new RefinableStructure(elems,
        loc,
        firstup2,
        mid1up1,
        mid2up1,
        endup1,
        ufirst,
        uend,
        sidxup1,
        uidxup1,
        setsup1,
        bunches,
        items))

    }

  }

  def Split2(s: Int): (Int, RefinableStructure) = {
    if (mid1(s) == mid2(s)) {
      val mid2up = mid2.updated(s, end(s))
      val nuovaRef = new RefinableStructure(elems,
        loc,
        first,
        mid1,
        mid2up,
        end,
        ufirst,
        uend,
        sidx,
        uidx,
        sets,
        bunches,
        items)
      (0, nuovaRef)
    }
    else if (mid2(s) == end(s)) {
      (0, this)
    }
    else {

      val setsup1 = sets + 1
      val uidxup1 = uidx :+ uidx(s)
      val firstup1 = first :+ mid2(s)
      val endup1 = end :+ end(s)
      val endup2 = endup1.updated(s, mid2(s))
      val mid1up1 = mid1 :+ firstup1(setsup1)
      val mid2up1 = mid2 :+ endup2(setsup1)

      val sidxup1 = iter(firstup1(setsup1), endup2(setsup1) - 1, setsup1,
        sidx, elems)

      (setsup1, new RefinableStructure(elems,
        loc,
        firstup1,
        mid1up1,
        mid2up1,
        endup2,
        ufirst,
        uend,
        sidxup1,
        uidxup1,
        setsup1,
        bunches,
        items
      ))

    }


  }

  def No_marks(s: Int): Boolean = {
    if ((mid1(s) == first(s)) && (mid2(s) == end(s))) true
    else false
  }

  def Bunch(s: Int): Int = uidx(s)

  def Bunch_first(u: Int): Int = elems(ufirst(u))

  def Bunch_next(e: Int): Int = {
    if ((loc(e) + 1) >= uend(uidx(sidx(e))))
      0
    else
      elems(loc(e) + 1)

  }

  def Has_many(u: Int): Boolean = {
    if (end(sidx(elems(ufirst(u)))) != uend(u)) true
    else
      false
  }

  def Extract_set(u: Int): (Int, RefinableStructure) = {


    val s1 = sidx(elems(ufirst(u)))
    val s2 = sidx(elems(uend(u) - 1))



    if (s1 == s2) {
      (0, this)
    }
    else {
      val bunchesup1 = bunches + 1

      if (Size(s1) <= Size(s2)) {
        val ufirstup1 = ufirst.updated(u, end(s1))
        val ufirstup2 = ufirstup1 :+ first(s1)
        val uendup1 = uend :+ end(s1)
        val uidxup1 = uidx.updated(s1, bunchesup1)
        (s1,
          new RefinableStructure(elems,
            loc,
            first,
            mid1,
            mid2,
            end,
            ufirstup2,
            uendup1,
            sidx,
            uidxup1,
            sets,
            bunchesup1,
            items))
      }
      else {

        val uendup1 = uend.updated(u, first(s2))
        val ufirstup1 = ufirst :+ first(s2)
        val uendup2 = uendup1 :+ end(s2)
        val uidxup1 = uidx.updated(s2, bunchesup1)
        (s2, new RefinableStructure(elems,
          loc,
          first,
          mid1,
          mid2,
          end,
          ufirstup1,
          uendup2,
          sidx,
          uidxup1,
          sets,
          bunchesup1,
          items))


      }


    }


  }

  def Left_neighbour(e: Int): Int = {
  val l = first(sidx(e))
    if (l > 1)
      elems(l - 1)
    else
      0
  }

  def Right_neighbour(e:Int): Int = {
    val l = end(sidx(e))

    if (l <= items)
      elems(l)
    else
      0

  }


}
