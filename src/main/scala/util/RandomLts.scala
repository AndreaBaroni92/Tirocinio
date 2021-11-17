package util
import lts._

import java.io.{File, FileWriter}
import scala.annotation.tailrec
class RandomLts(fileName:String,numStates:Int,numLabel:Int,numTransitions:Int) {


  val nodes: List[String] = (0 until numStates).map(x => "q"++x.toString).toList
  val transitions = (0 until numTransitions).map(_ =>makesingleTrans(numStates,numLabel)).toList
    .distinctBy((a)=>a._1++"-"++a._2++"-"++a._3 )
    .map(x => "q"++x._1++",l"++x._2++",q"++x._3)


  @tailrec
   final def makeLine(acc:String, l:List[String], name:String, sep:String):String = {

    l match {
      case ::(_, _) => {
        val l2 = name++"="++l.take(10).mkString(sep)++"\n"
        makeLine(acc++l2,l.drop(10),name,sep)
      }
      case Nil => acc
    }

  }

  def makesingleTrans(numbState:Int,numbLabel:Int):(String,String,String)={
    val tail = scala.util.Random.nextInt(numbState ).toString
    val label = scala.util.Random.nextInt(numbLabel  ).toString
    val head = scala.util.Random.nextInt(numbState ).toString
    (tail,label,head)
  }

  def stampa(): Unit ={

    val fileToWrite = new File(fileName)


    if(! fileToWrite.getParentFile.exists() ){
      fileToWrite.getParentFile.mkdirs()
    }

    val fileWriter = new FileWriter(fileToWrite)
    val s1 = makeLine("",nodes,"states",",")
    val s2 = makeLine("",transitions,name="relation",";")
    val ris = s1 ++"\n"++s2
    fileWriter.write(ris)
    fileWriter.close()

  }


}

