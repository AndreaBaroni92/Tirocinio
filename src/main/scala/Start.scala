import bisimulation._
import valmUtil._
import lts.{Label, Node}
import util.processInp
import valmUtil.{Bunch, Refinable, ThreeSet}


object Start {
  def main(args: Array[String]): Unit = {


    val myLts = processInp.getLts("src/main/scala/inp")

    println(myLts)

    val alg = new FixedPoint(myLts)

    val alg2 = new KannelakisSmolka(myLts)

    //println(alg.KannelakisSmolka())


    //println(alg.fixedPointBisim())
    //println(alg2.compute())

    val prova = new Valmari(myLts)

    //println(prova.OutsetRefinable)
    //println(prova.splitRef)
    println(prova.BlockRef)


    val tobemarked:List[Item] = List(State(new Node("s0")),State(new Node("t1")))
    val marcati = prova.listmark1(prova.BlockRef,tobemarked)

    println(marcati)


    val splittato = prova.split1(marcati)
    val tobemarked2:List[Item] = List(State(new Node("t1")),State(new Node("s0")),State(new Node("t4")))
    val marcati2=  prova.listmark1(splittato,tobemarked2)
    println(marcati2)
    println(prova.split1(marcati2))

  }
}
