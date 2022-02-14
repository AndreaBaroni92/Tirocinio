package lts

class Lts(val nodes: List[Node], val transRel: TransitionRelation) {


  val indexNodes: Map[String, Int] = nodes.map(x => x.name).zipWithIndex.toMap

  val fromIntToNode: Map[Int, String] = indexNodes.map(x => (x._2, x._1))

  val listLab: List[String] = transRel.rel.map(x => x._2.name).distinct

  val numLabel: Int = listLab.length
  val indexTrans: Map[String, Int] = listLab.zipWithIndex.toMap

  val numIndex: List[Int] = indexTrans.values.toList

  val fromIntToLab: Map[Int, String] = indexTrans.map(x => (x._2, x._1))

  val transitionNumber: List[(Int, Int, Int)] = transRel.rel.map(x => {
    val q1 = indexNodes getOrElse(x._1.name,-1)
    val a = indexTrans getOrElse(x._2.name,-1)
    val q2 = indexNodes getOrElse(x._3.name,-1)
    (q1,a,q2)
  })

  val numNodes: Int = nodes.length
  val numTrans: Int = transRel.rel.length

  val mapTrans: Map[Int, List[(Int, Int, Int)]] = transitionNumber.groupBy(x => x._1)

  val vectorTrans: Vector[Vector[Vector[(Int, Int, Int)]]] = (0 until numNodes).toVector.map(x => (0 until numLabel).toVector.map(y => {
    val l1 = (mapTrans getOrElse(x,Nil)).toVector
    l1.filter(el=> el._2 == y)
  }) )


  // mapnode restituisce le transizioni che partono da un nodo sorgente
  val mapNode: Map[String, List[(Node, Label, Node)]] = transRel.rel.groupBy(x => x._1.name)

  val transitionVector: Vector[(Node, Label, Node)] = transRel.rel.toVector


  // getLabel restituisce una lista di nodi destinazione che sono raggiungibili
  // a partire da un nodo sorgente (node) e da una label (label)

  val it: Map[String, List[(Node, Label, Node)]] = transRel.rel.groupBy(x => x._3.name)
  def InTransition(s:Node): List[(Node, Label, Node)] ={
    it.getOrElse(s.name,Nil)
    //transRel.rel.filter(x => x._3.name == s.name)
  }
  def getLabel(node: Node, label: Label): List[Node] = {

    mapNode.get(node.name) match {
      case Some(value) => value.filter(x => x._2.name == label.name).map(n => n._3)
      case None => Nil
    }

  }

  def deadlockStates:List[Node] = {
    val l1:List[Node] = transRel.rel.distinctBy(x => x._1.name).map(x => x._1)
    nodes.filterNot(x => l1.exists(n => n.name == x.name))
  }

  override def toString: String = s"Nodi:\n" +
    s"${nodes mkString ","} " +
    s"\nRelazione di transizione:" +
    s"\n${transRel}"

}
