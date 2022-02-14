package util

import java.io.{File, FileWriter}

object GenLts {

  val basePath: String = "C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\"

  /*
  createBench crea un insieme di lts con le caratteristiche definite dai parametri
   */
  def createBench(numStates: Int, numLabel: Int, numTransitions: Int, numSamples: Int): Unit = {
    val ltsName = "lts" ++ numStates.toString ++ "_" ++ numLabel.toString ++ "_" ++ numTransitions.toString

    val dirname = basePath ++ ltsName ++ "\\"

    (1 to numSamples).foreach(x => {
      val l = new RandomLts(dirname ++ ltsName ++ "_" ++ x.toString, numStates, numLabel, numTransitions)
      l.stampa()
    })

  }


  def main(args: Array[String]): Unit = {
    //val l = new RandomLts("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\prova\\inp100034000",2000,3,8000)

    //l.stampa()

    //val f = new File("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\prova2\\inp100034000")
    //println(f.getParentFile().mkdirs())
    //println(f.getParent())

   // (1 to 20).foreach(i =>createBench(i * 1000, 10,20000, 10))

    createBench(10000, 10,500000, 1)
    //val x = new File("C:\\Users\\utente\\Desktop\\160711\\src\\main\\scala\\lts20_2_10")
    //println(x.listFiles().mkString(","))

  }
}
