package valmUtil

class Bunch(val b:List[ThreeSet]){

  private val setIndex = b.zipWithIndex

  private val toPrint = setIndex.map( x => "\nSet "
    ++ x._2.toString
    ++ "= {\n"
    ++ x._1.toString
    ++ "\n}"

  )

  override def toString: String = toPrint.mkString(",")


}