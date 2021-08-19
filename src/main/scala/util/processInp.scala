package util

import lts.{Label, Lts, Node, TransitionRelation}

import scala.io.Source

object processInp {
  private def readState(filename: String) = {
    val source = Source.fromFile(filename)
    val states = (for {
      line <- source.getLines()
      if line.startsWith("states=")
      listOfStates = line.substring("states=".length)
      states = listOfStates.split(",")
      name <- states
      ris = new Node(name)
    } yield ris).toList
    source.close()
    states
  }

  private def readTransitions(filename: String) = {
    val source = Source.fromFile(filename)
    val transitionRelationLts = (for {
      line <- source.getLines()
      if line.startsWith("relation=")
      rel = line.substring("relation=".length)
      relation = rel.split(";")
      elem <- relation
      singleElement = elem.split(",") //singleElement rappresenta la tripla(source , action, target)

    } yield (new Node(singleElement(0)), new Label(singleElement(1)), new Node(singleElement(2)))).toList
    source.close()
    new TransitionRelation(transitionRelationLts)
  }

  def getLts(filename: String) = {
    new Lts(readState(filename), readTransitions(filename))
  }

}
