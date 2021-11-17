import ValmVector.{Bisim, Init}
import bisimulation._
import util.processInp
import util.Measure
import util.Measure.time

import java.io.File
import scala.annotation.tailrec


object Start {


  def timeBisim(dir: String, timeAlg: String => Double): Double = {

    val samples = new File(dir).listFiles().toList

    @tailrec
    def mediumTime(total: Double, files: List[File]): Double = {

      files match {
        case ::(head, next) => {


          val elapsedTime = timeAlg(head.toString)


          println(s"Elapsed time : ${elapsedTime}")
          mediumTime(elapsedTime + total, next)

        }
        case Nil => total
      }

    }


    mediumTime(0, samples) / samples.length


  }

  def doKS(ltsPath: String): Double = { // restituisce il tempo in millisecondi per algoritmo Kannellakis
    val myLts = processInp.getLts(ltsPath)
    val t0 = System.nanoTime()
    val alg2 = new KannelakisSmolka(myLts)
    val risult = alg2.compute()
    println(s"Numero classi Kannelakis: ${risult.length}")
    //println(risult)
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000
  }

  def doFixedPoint(ltsPath: String): Double = {
    val myLts = processInp.getLts(ltsPath)
    val t0 = System.nanoTime()
    val alg = new FixedPoint(myLts)
    val risult = alg.fixedPointBisim()
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000
  }

  def doValmari(ltsPath: String): Double = {
    val myLts = processInp.getLts(ltsPath)
    val t0 = System.nanoTime()
    val temp1 = System.nanoTime()
    val i = new Init(myLts)
    val b = i.initBlock()
    val o = i.initOutsets()
    val s = i.initSplitter()
    val inTrans = (x: Int) => i.In_transitions(x)
    val bisim = new Bisim(b, s, o, i.tail, inTrans)
    val temp2 = System.nanoTime()
    println(s"Tempo inizializzazione:  ${(temp2 - temp1) / 1000000}")
    val ref = bisim.computeBisim()
    val _ = i.printSet(ref)
    println(s"Numero sets Valmari:  ${ref.sets}")
    //println(risult)
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000


  }

  def main(args: Array[String]): Unit = {


    // l'argomento di getlts e' il path dove e' presente il labeled transition systems
    val myLts = processInp.getLts("")


    //println(myLts.deadlockStates)
    //println(myLts)
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


    val inTrans = (x: Int) => i.In_transitions(x)


    println("start")
    val bisim = new Bisim(b, s, o, i.tail, inTrans)
    /*
        println(i.printSet(bisim.prova._1))
        println(i.printSetOfTransition(bisim.prova._2))
        println(i.printSetOfTransition(bisim.prova._3))
    */
    //    println("Unready",bisim.prova._4)


    println(i.printSet(bisim.computeBisim()))


    println("ks")
    val alg2 = new KannelakisSmolka(myLts)

    println(alg2.compute())
    /*
        val alg = new FixedPoint(myLts)
        println(alg.fixedPointBisim())*/


    // il primo argomento di timebisim e' il path della directory dove sono presenti i labeled transition systems
    val tempoKannelakis = timeBisim("",
      (s: String) => doKS(s))
    val tempoValmari = timeBisim("",
      (s: String) => doValmari(s))




    // val tempoFixed = timeBisim("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\lts1000_2_100",(s:String)=> doFixedPoint(s))


    println(s"Tempo media in millisecondi Kannelakis : $tempoKannelakis")
    println(s"Tempo media in millisecondi Valmari: $tempoValmari")
    // println(s"Tempo media in millisecondi PuntoFisso: $tempoFixed" )


  }
}
