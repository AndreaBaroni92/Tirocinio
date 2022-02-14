package BPP


class BPPnet(val s:List[Place],val t:List[Transition]) {

  val indexPlaces: Map[String, Int] = s.map(x => x.name).zipWithIndex.toMap

  val numPlaces: Int = s.length

  val listLab: List[String] = t.map(x => x.l.name).distinct

  val numLab: Int = listLab.length

  val indexTrans: Map[String, Int] = listLab.zipWithIndex.toMap

  val numOfTransition: List[Int] = indexTrans.values.toList

  val transitionNumber: List[(Int, Int, MultiSet)] = t.map(x => {
    val q1 = indexPlaces getOrElse(x.in.name,-1)
    val q2 = indexTrans getOrElse(x.l.name,-1)
    (q1,q2,x.o)
  })

  val mapTrans: Map[Int, List[(Int, Int, MultiSet)]] = transitionNumber.groupBy(x => x._1)

  val vectorTrans: Vector[Vector[List[MultiSet]]] = (0 until numPlaces).toVector.map(x => (0 until numLab).toVector.map(y => {
    val l1 = (mapTrans getOrElse(x,Nil))
    l1.filter(el => el._2 == y).map(x => x._3)
  }))

}
