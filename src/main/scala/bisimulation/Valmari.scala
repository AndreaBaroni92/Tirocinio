package bisimulation

import valmUtil._
import lts.{Label, Lts, Node, TransitionRelation}

class Valmari(val l: Lts) {

  // crea un insieme con tutti i nodi unmarked
  val BlockSet = new ThreeSet(Nil, Nil, l.nodes.map(x => State(x)))

  // crea un Bunch che è una lista composta da un solo elemento che e' l'insieme S
  //di tutti i nodi iniziali
  val BlockBunch = new Bunch(List(BlockSet))

  // crea una refinable partition data structures composta da una lista
  // la quale e' composta da un solo Bunch
  val BlockRef = new Refinable(List(BlockBunch))

  // start splitter raggruppa le transizioni secondo le Label
  val startSplitter = l
    .transRel
    .rel
    .map(x => Transition(x))
    .groupBy(x => x.t._2.name)
    .values
    .toList
  //.map(t => new TransitionRelation(t))


  // SplitSet per ogni transizione raggruppata crea un set
  val SplitSet: List[ThreeSet] = startSplitter
    .map(x => new ThreeSet(Nil, Nil, x))
  // per ogni set viene creato un bunch
  val splitBunch: List[Bunch] = SplitSet.map(x => new Bunch(List(x)))
  val splitRef = new Refinable(splitBunch)


  val startOutsets = l
    .transRel
    .rel
    .groupBy(x => x._1.name)
    .values
    .toList.flatMap(x => x
    .groupBy(elem => elem._2.name)
    .values
    .toList)
    .map(x => new ThreeSet(Nil, Nil, x.map(x1 => Transition(x1))))
  //.map(x => new Bunch(List(x)))

  val OutsetRefinable = new Refinable(List(new Bunch(startOutsets)))


  //marca elemento i dentro to Update
  def mark1(toUpdate: Refinable, i: Item): Refinable = {

    def iterItem(listUnmark: List[Item]): List[Item] = {

      listUnmark.filterNot((itun: Item) => { // guarda se nella lista di unmarked e' presente i
        itun match {
          case State(s) => i match {
            case State(s1) => s.name == s1.name
            case Transition(_) => false
          }
          case Transition(t) => false // TODO da fare questo caso
        }
      })

    }

    def checkm1orm2(listmark1: List[Item], listmark2: List[Item]): Boolean = {

      val ris1 = listmark1.exists(m1 => m1 match {
        case State(s) => i match {
          case State(s1) => s.name == s1.name
          case Transition(_) => false // si sta scorrendo nodi quindi non si puo' confrontare transizioni con nodi
        }
        case Transition(_) => false //todo da fare questo caso
      })

      val ris2 = listmark2.exists(m1 => m1 match {
        case State(s) => i match {
          case State(s1) => s.name == s1.name
          case Transition(_) => false // si sta scorrendo nodi quindi non si puo' confrontare transizioni con nodi
        }
        case Transition(_) => false //todo da fare questo caso
      })

      ris1 || ris2
    }

    def iterSets(listSets: List[ThreeSet]) = {


      def updateListSet(updated: List[ThreeSet], toUpdate: List[ThreeSet]): (List[ThreeSet], Boolean) = {

        toUpdate match {
          case ::(head, next) => {
            if (head.um.length != iterItem(head.um).length) { // e' stato fatto un aggiornamento
              val ris = new ThreeSet(i :: head.m1, head.m2, iterItem(head.um))
              val newList = (ris :: next) ::: updated
              (newList, true)

            }
            else if (checkm1orm2(head.m1, head.m2)) { // i e' gia' in m1 o in m2
              (toUpdate ::: updated, true)
            }
            else {
              updateListSet(head :: updated, next)
            }

          }
          case Nil => (updated, false)
        }

      }

      updateListSet(Nil, listSets)

    }

    def iterBunch(listbunch: List[Bunch]) = {


      def updateListBunch(updated: List[Bunch], toUpdate: List[Bunch]): List[Bunch] = {

        toUpdate match {
          case ::(head, next) => {

            if (iterSets(head.b)._2) { // e stato fatto un aggiornamento nella lista di bunch
              val upbunch = new Bunch(iterSets(head.b)._1)
              (upbunch :: next) ::: updated
            }
            else {
              updateListBunch(head :: updated, next)
            }

          }
          case Nil => updated
        }


      }

      updateListBunch(Nil, listbunch)

    }


    val nuovalista = iterBunch(toUpdate.r)
    new Refinable(nuovalista)
  }

  // marca con la marca 1 in update  tutti gli item appartenenti all lista tobemarked
  def listmark1(update: Refinable, tobemarked: List[Item]): Refinable = {

    tobemarked match {
      case ::(head, next) => listmark1(mark1(update, head), next)
      case Nil => update
    }

  }

  def statesfrombunch(bu: Bunch): List[Item] = { //restituisce tutti gli elementi(item) di un bunch

    bu.b.flatMap(x => (x.m2 ::: x.m1 ::: x.um))

  }


  // splitta tutti i nodi marcati con1
  def split1(r: Refinable) = {

    def checkplit1Set(s: ThreeSet): Boolean = {
      if (s.m1.isEmpty || s.um.isEmpty) {
        //s
        false
      }
      else {
        //val newset1 = new ThreeSet(Nil,Nil,s.m1)
        //val newset2 = new ThreeSet(Nil,s.m2,s.um)
        true
      }

    }


    def iterThreeSet(l: List[ThreeSet]): List[ThreeSet] = {

      def scanthreeset(updated: List[ThreeSet], toupdate: List[ThreeSet]): List[ThreeSet] = {
        toupdate match {
          case ::(head, next) =>{
            if (checkplit1Set(head)) {
              val newset1 = new ThreeSet(Nil, Nil, head.m1)
              val newset2 = new ThreeSet(Nil, head.m2, head.um)
              scanthreeset(newset1 :: newset2 :: updated, next)
            }
            else {
              val newset = new ThreeSet(Nil,Nil,head.m1:::head.m2:::head.um)
              scanthreeset(newset :: updated, next)
            }
          }
          case Nil => updated
        }
      }
      scanthreeset(Nil,l)

    }

    def iterBunch(l:List[Bunch]) = {

      def scanBunch(updated:List[Bunch],toupdate:List[Bunch]):List[Bunch] = {

        toupdate match {
          case ::(head, next) => {
            val nuovobunch = new Bunch(iterThreeSet(head.b))
            scanBunch(nuovobunch::updated,next)
          }
          case Nil =>updated
        }


      }

      scanBunch(Nil,l)


    }

    new Refinable(iterBunch(r.r))

  }


  // con questa funzione viene marcato ogni nodo in block che puo'
  // permettersi una a-labeled transition
  def processSplitterBunch(listsplitterbunchupdated: List[Bunch],
                           markedBlock: Refinable): Refinable = {

    listsplitterbunchupdated match {
      case ::(head, next) => {
        val updateref1 = listmark1(markedBlock, statesfrombunch(head))
        //todo una volta marcati si deve fare split e update per ogni set

        val updateref2 = split1(updateref1)
        processSplitterBunch(next, updateref2)

      }
      case Nil => markedBlock
    }

  }


}

