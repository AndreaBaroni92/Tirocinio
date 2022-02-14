import ValmVector.{Bisim, Init}
import bisimulation._
import util.processInp
import util.Measure
import util.Measure.time

import java.io.File
import scala.annotation.tailrec


object Start {


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
    println("----------")
    val t0 = System.nanoTime()
    val i = new Init(myLts)
    val b = i.initBlock()
    val o = i.initOutsets()
    val s = i.initSplitter()
    //val inTrans = (x: Int) => i.In_transitions(x)
    val inTrans = (x: Int) => i.newIn_tras(x)
    val bisim = new Bisim(b, s, o, i.tail, inTrans)
    val ref = bisim.computeBisim()
    val _ = i.printBisimEquivalence(ref)
    println(s"Numero sets Valmari:  ${ref.sets}")
    println("\n")
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000


  }

  def main(args: Array[String]): Unit = {


    val tempoValmari = timeBisim("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\lts10000_10_50000",
      (s: String) => doValmari(s))

    val tempoKannelakis5 = timeBisim("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\lts10000_10_50000",
      (s: String) => doKS5(s))


    println(s"Tempo media in millisecondi Valmari: $tempoValmari")
    println(s"Tempo media in millisecondi Kannelakis5 : $tempoKannelakis5")

  }

  def timeBisim(dir: String, timeAlg: String => Double): Double = {

    val samples = new File(dir).listFiles().toList

    @tailrec
    def mediumTime(total: Double, files: List[File]): Double = {

      files match {
        case ::(head, next) => {


          val elapsedTime = timeAlg(head.toString)


          println(s"File : ${head.toString}")
          println(s"Elapsed time : ${elapsedTime}")
          println("--------------\n")
          mediumTime(elapsedTime + total, next)

        }
        case Nil => total
      }

    }


    mediumTime(0, samples) / samples.length


  }


  def doKS5(ltsPath: String): Double = { // restituisce il tempo in millisecondi per algoritmo Kannellakis

    println("----------")
    val t0 = System.nanoTime()
    val alg2 = KS5.compBisim(ltsPath)
    println(s"Numero classi Kannelakis 5: ${alg2.length}")
    //println(risult)
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000
  }


  def doKS6(ltsPath: String): Double = { // restituisce il tempo in millisecondi per algoritmo Kannellakis

    val t0 = System.nanoTime()
    val alg2 = KS6.compBisim(ltsPath)

    println(s"Numero classi Kannelakis 6: ${alg2.length}")
    //println(risult)
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000
  }
}


