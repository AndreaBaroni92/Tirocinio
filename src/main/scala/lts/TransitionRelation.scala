package lts

class TransitionRelation(val rel: List[(Node, Label, Node)]) {
  //override def toString: String = s"{${rel mkString " , "}}"
  val newRel = rel
    .map(x => "("
      .concat(x._1.toString)
      .concat("--")
      .concat(x._2.toString)
      .concat("-->")
      .concat(x._3.toString)
      .concat(")")).mkString(",")
  override def toString: String = s"{$newRel}"

}