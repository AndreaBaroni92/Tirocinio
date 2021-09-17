import ValmVector.{Bisim, Init}
import bisimulation._
import valmUtil._
import lts.{Label, Node}
import util.processInp
import valmUtil.{Bunch, Refinable, ThreeSet}


object Start {
  def main(args: Array[String]): Unit = {


    val myLts = processInp.getLts("src/main/scala/inp")


    println(myLts)
/*
    val alg = new FixedPoint(myLts)

    val alg2 = new KannelakisSmolka(myLts)

    //println(alg.KannelakisSmolka())


    println(alg.fixedPointBisim())
    println(alg2.compute())

    val prova = new Valmari(myLts)

    println(prova.compute)
*/


    val i = new Init(myLts)

    //val s = i.initSplitter().Mark1(3).Split1(2)._2.Mark1(3).Split1(3)._2.Extract_set(1)._2
    val b = i.initBlock()
    val o = i.initOutsets()
    val s = i.initSplitter()


    val inTrans= (x:Int)=> i.In_transitions(x)




    val bisim = new Bisim(b,s,o,i.tail,inTrans)

    println(i.printSet(bisim.prova._1))
    println(i.printSetOfTransition(bisim.prova._2))
    println(i.printSetOfTransition(bisim.prova._3))

    println("Unready",bisim.prova._4)

    println(i.printSet(bisim.ris))

    val alg2 = new KannelakisSmolka(myLts)
    println(alg2.compute())





  }
}
