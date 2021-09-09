package valmUtil

class Refinable(val r:List[Bunch]) {

  private val bunchIndex = r.zipWithIndex

  private val toPrint =bunchIndex.map( x => "Bunch "
    ++ x._2.toString
    ++ "= {"
    ++ x._1.toString
    ++ "\n}"

  )

  override def toString: String = toPrint.mkString(",\n")

}
