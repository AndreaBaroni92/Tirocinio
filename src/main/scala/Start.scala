import bisimulation._
import valmUtil._
import lts.{Label, Node}
import util.processInp
import valmUtil.{Bunch, Refinable, ThreeSet}


object Start {
  def main(args: Array[String]): Unit = {


    val myLts = processInp.getLts("src/main/scala/inp2")

    println(myLts)

    val alg = new FixedPoint(myLts)

    val alg2 = new KannelakisSmolka(myLts)

    //println(alg.KannelakisSmolka())


    println(alg.fixedPointBisim())
    println(alg2.compute())

    val prova = new Valmari(myLts)

    println(prova.compute)

  }
}
