package valmUtil

class ThreeSet(val m1:List[Item], val m2:List[Item],val um:List[Item]){
  def show(name:String,x:List[Item]): String = {
    val l = x.map(e => e.toString)
    name.concat(
      "{".concat(l.mkString(",")).concat("}")
    )
  }



  override def toString: String = show("marked1: ",m1)
    .concat("\n")
    .concat(show("marked2: ",m2))
    .concat("\n")
    .concat(show("unmarked: ",um))

}