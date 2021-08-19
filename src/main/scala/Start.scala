import bisimulation._
import util.processInp


object Start {
  def main(args: Array[String]): Unit = {


    val myLts = processInp.getLts("src/main/scala/inp2")

    println(myLts)

    val alg = new FixedPoint(myLts)

    val alg2 = new KannelakisSmolka(myLts)

    //println(alg.KannelakisSmolka())


    println(alg.fixedPointBisim())
    println(alg2.compute())

  }
}
