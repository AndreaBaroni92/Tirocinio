package BPP

class MultiSet(val m:List[(Int,Place)]) {

  private val toPrint = m.map(x => s"${x._1.toString ++ "*"++x._2.toString++"\n" }").mkString("")
  override def toString: String = toPrint

  val len: Int = m.length
}

