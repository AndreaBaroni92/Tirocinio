package lts

class Lts(val nodes: List[Node], val transRel: TransitionRelation) {

  // mapnode restituisce le transizioni che partono da un nodo sorgente
  val mapNode: Map[String, List[(Node, Label, Node)]] = transRel.rel.groupBy(x => x._1.name)

  val listLab = transRel.rel.map(x => x._2.name).distinct

  // getLabel restituisce una lista di nodi destinazione che sono raggiungibili
  // a partire da un nodo sorgente (node) e da una label (label)

  def getLabel(node: Node, label: Label): List[Node] = {

    mapNode.get(node.name) match {
      case Some(value) => value.filter(x => x._2.name == label.name).map(n => n._3)
      case None => Nil
    }

  }


  override def toString: String = s"Nodi:\n" +
    s"${nodes mkString ","} " +
    s"\nRelazione di transizione:" +
    s"\n${transRel}"

}
