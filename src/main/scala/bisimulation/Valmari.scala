package bisimulation

import lts.Lts
import valmUtil._

import scala.annotation.tailrec

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
  val startSplitter: List[List[Transition]] = l
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
  def mark(toUpdate: Refinable, i: Item, markFlag: Boolean): Refinable = {

    def iterItem(listUnmark: List[Item]): List[Item] = {

      listUnmark.filterNot((itun: Item) => { // guarda se nella lista di unmarked e' presente i
        itun match {
          case State(s) => i match {
            case State(s1) => s.name == s1.name
            case Transition(_) => false
          }
          case Transition(trans1) => {
            i match {
              case State(_) => false
              case Transition(t2) => trans1._1.name == t2._1.name &&
                trans1._2.name == t2._2.name &&
                trans1._3.name == t2._3.name
            }
          }
        }
      })

    }

    def checkm1orm2(listmark1: List[Item], listmark2: List[Item]): Boolean = {

      val ris1 = listmark1.exists(m1 => m1 match {
        case State(s) => i match {
          case State(s1) => s.name == s1.name
          case Transition(_) => false // si sta scorrendo nodi quindi non si puo' confrontare transizioni con nodi
        }
        case Transition(t2) => {
          i match {
            case State(_) => false
            case Transition(t3) => t2._1.name == t3._1.name &&
              t2._2.name == t3._2.name &&
              t2._3.name == t3._3.name
          }
        }
      })

      val ris2 = listmark2.exists(m1 => m1 match {
        case State(s) => i match {
          case State(s1) => s.name == s1.name
          case Transition(_) => false // si sta scorrendo nodi quindi non si puo' confrontare transizioni con nodi
        }
        case Transition(t2) => {
          i match {
            case State(_) => false
            case Transition(t3) => t2._1.name == t3._1.name &&
              t2._2.name == t3._2.name &&
              t2._3.name == t3._3.name
          }
        }
      })

      ris1 || ris2
    }

    def iterSets(listSets: List[ThreeSet]) = {


      def updateListSet(updated: List[ThreeSet], toUpdate: List[ThreeSet]): (List[ThreeSet], Boolean) = {

        toUpdate match {
          case ::(head, next) => {
            if (head.um.length != iterItem(head.um).length) { // e' stato fatto un aggiornamento
              if (markFlag) { // a seconda della marca inserisce in m1 o in m2
                val ris = new ThreeSet(i :: head.m1, head.m2, iterItem(head.um))
                val newList = (ris :: next) ::: updated
                (newList, true)
              }
              else {
                val ris = new ThreeSet(head.m1, i :: head.m2, iterItem(head.um))
                val newList = (ris :: next) ::: updated
                (newList, true)
              }


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
  def listmark(update: Refinable, tobemarked: List[Item], markFlag: Boolean): Refinable = {

    tobemarked match {
      case ::(head, next) => listmark(mark(update, head, markFlag), next, markFlag)
      case Nil => update
    }

  }

  def statesfrombunch(bu: Bunch): List[Item] = {
    //restituisce tutti gli elementi(item) di un bunch
    // nel caso il Bunch sia composto da stati restituisce gli stati
    // altrimenti se e' composto da transizioni restituisce la testa di una transizione
    // ovvero (head,label,tail)

    val ris = bu.b.flatMap(x => (x.m2 ::: x.m1 ::: x.um))

    ris.map {
      case State(s) => State(s)
      case Transition(t) => State(t._1)
    }

  }


  // splitta tutti i nodi marcati splitFlag, se splitflag = true => marca con 1
  // altrimenti se splitFalg = false => marca con 2
  def split(r: Refinable, splitFlag: Boolean) = {

    def checkplitSet(s: ThreeSet): Boolean = {

      if (splitFlag) {
        if (s.m1.isEmpty || s.um.isEmpty) {
          false
        }
        else {
          true
        }
      }
      else {
        if (s.m2.isEmpty || s.um.isEmpty) {
          false
        }
        else {
          true
        }

      }

    }


    def iterThreeSet(l: List[ThreeSet]): List[ThreeSet] = {

      def scanthreeset(updated: List[ThreeSet], toupdate: List[ThreeSet]): List[ThreeSet] = {
        toupdate match {
          case ::(head, next) => {
            if (checkplitSet(head)) {
              if (splitFlag) {
                val newset1 = new ThreeSet(Nil, Nil, head.m1)
                val newset2 = new ThreeSet(Nil, head.m2, head.um)
                scanthreeset(newset1 :: newset2 :: updated, next)
              }
              else {
                val newset1 = new ThreeSet(Nil, Nil, head.m2)
                val newset2 = new ThreeSet(head.m1, Nil, head.um)
                scanthreeset(newset1 :: newset2 :: updated, next)
              }

            }
            else {
              if (splitFlag) { // se splitflag => true
                val newset = new ThreeSet(Nil, head.m2, head.m1 ::: head.um)
                scanthreeset(newset :: updated, next)
              }
              else { // se splitflag => false
                val newset = new ThreeSet(head.m1, Nil, head.m2 ::: head.um)
                scanthreeset(newset :: updated, next)
              }

            }
          }
          case Nil => updated
        }
      }

      scanthreeset(Nil, l)

    }

    def iterBunch(l: List[Bunch]) = {

      def scanBunch(updated: List[Bunch], toupdate: List[Bunch]): List[Bunch] = {

        toupdate match {
          case ::(head, next) => {
            val nuovobunch = new Bunch(iterThreeSet(head.b))
            scanBunch(nuovobunch :: updated, next)
          }
          case Nil => updated
        }


      }

      scanBunch(Nil, l)


    }

    new Refinable(iterBunch(r.r))

  }


  def threeSetFromRefinable(re: Refinable): List[ThreeSet] = {
    re.r.flatMap(x => x.b)
  }

  def updateRefinableStructure(toUpdate: Refinable, updated: Refinable): Refinable = {
    update(toUpdate, threeSetFromRefinable(updated))
  }

  // aggiorna la struttura r a essere coerente con la lista di set
  def update(r: Refinable, listThreeSet: List[ThreeSet]): Refinable = {


    @tailrec
    def scan(newref: Refinable, toscann: List[ThreeSet]): Refinable = {
      toscann match {
        case ::(head, next) => {
          val transizionidamarcare = transitionfromsinglethreeset(head)
          val nuovarefinable = listmark(newref, transizionidamarcare, true)
          val nuovasplit = split(nuovarefinable, true)
          scan(nuovasplit, next)
        }
        case Nil => newref
      }
    }

    def transitionfromsinglethreeset(threeset: ThreeSet): List[Item] = {

      val ris = threeset.um ::: threeset.m2 ::: threeset.m1


      ris.flatMap(x => {
        x match {
          case State(s) => {
            val intrans = l.InTransition(s)
            intrans.map(x => Transition(x))
          }
          case Transition(t) => List(Transition(t)) //non e' mai il caso
        }
      })
    }

    scan(r, listThreeSet)
  }

  // con questa funzione viene marcato ogni nodo in block che puo'
  // permettersi una a-labeled transition
  def processSplitterBunch(listsplitterbunchupdated: List[Bunch],
                           markedBlock: Refinable): Refinable = {

    listsplitterbunchupdated match {
      case ::(head, next) => {

        val updateref1 = listmark(markedBlock, statesfrombunch(head), true)
        //todo una volta marcati si deve fare split e update per ogni set

        val updateref2 = split(updateref1, true)

        processSplitterBunch(next, updateref2)

      }
      case Nil => markedBlock
    }

  }


  /*
  con la seguente funzione vengono processati tutti i bunch della struttura splitter che hanno piu'
  di due insiemi
   */
  def processUnreadyBunches(splitter: Refinable, blocks: Refinable): Refinable = {


    @tailrec
    def scanBunch(updated: List[Bunch], toupdate: List[Bunch]): Option[(ThreeSet, List[ThreeSet], Refinable)] = {
      toupdate match {
        case ::(head, next) => {
          if (head.b.length >= 2) {
            val p :: u = head.b
            val nuovoBunch1 = new Bunch(List(p))
            val nuovoBunch2 = new Bunch(u)
            val strutturaAggiornata = new Refinable((nuovoBunch1 :: nuovoBunch2 :: next) ::: updated)
            Some((p, u, strutturaAggiornata))
          }
          else {
            scanBunch(head :: updated, next)
          }
        }
        case Nil => None
      }
    }

    val extractedSet = scanBunch(Nil, splitter.r)


    def decidem1orm2(i: Item, u: List[ThreeSet]) = {
      val listOfItemInU = u.flatMap(x => x.um ::: x.m1 ::: x.m2)

      val risultato = listOfItemInU.exists(it => it match {
        case State(_) => false // non e' mai  il caso in quanto listOfItemInU e'
        // una lista di transizioni
        case Transition((tl, l, _)) => i match {
          case State(_) => false // non e' mai  il caso
          case Transition((tl1, l1, _)) =>

            if (tl.name == tl1.name && l.name == l1.name) {
              true
            }
            else {

              false
            }
        }
      })
      //println("risultato: ",risultato,i,u)

      risultato
    }

    extractedSet match {
      case Some((p, u, strutturaAggiornata)) => {
        //to scan sono tutte le transizioni distinte per tail nell'insieme p estratto
        val toScan = (p.m1 ::: p.m2 ::: p.um).distinctBy(x => x match {
          case State(s) => s.name //non e' mai il caso
          case Transition((tail, _, _)) => tail.name
        })

        @tailrec
        def itemOfPToMark(listp: List[Item], blockUpdated: Refinable): Refinable = {
          listp match {
            case ::(head, next) => {
              val nodeFromItem: State = head match {
                case State(s) => State(s) // non e' mai il caso
                case Transition((nod, _, _)) => State(nod)
              }
              if (decidem1orm2(head, u)) {

                val nuovaRef = mark(blockUpdated, nodeFromItem, true) //marca 1 in quanto
                //c'e' una transizione in u
                itemOfPToMark(next, nuovaRef)
              }
              else {
                val nuovaRef = mark(blockUpdated, nodeFromItem, false)
                itemOfPToMark(next, nuovaRef)
              }
            }
            case Nil => blockUpdated
          }
        }

        val markedRefBlock = itemOfPToMark(toScan, blocks)

        val blockupdate1 = split(markedRefBlock, true)
        val blockupdate2 = split(blockupdate1, false)
        val splitterUpdated = updateRefinableStructure(strutturaAggiornata, blockupdate2)

        processUnreadyBunches(splitterUpdated, blockupdate2)


      }
      case None => {
        //println(splitter)
        blocks
      } // non ci sono piu' splitter con piu' di due set
    }


  }


  def compute: Refinable = { // calcola la relazione di bisimulazione

    val bloccoAggiornato = processSplitterBunch(splitRef.r,BlockRef)
    val splitterAggiornato = updateRefinableStructure(splitRef,bloccoAggiornato)
    processUnreadyBunches(splitterAggiornato,bloccoAggiornato)

  }

}

