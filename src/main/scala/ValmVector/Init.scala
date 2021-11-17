package ValmVector

import lts.{Label, Lts, Node}

import scala.annotation.tailrec

class Init(l: Lts) {

  val indexNodes: List[(String, Int)] = l
    .nodes
    .map(x => x.name)
    .zipWithIndex
    .map(x => (x._1, x._2 + 1))

  val mapNodeSting: Map[String, Int] = indexNodes.toMap
  val mapNodeInt: Map[Int, String] = indexNodes.map(x => (x._2, x._1)).toMap

  val indexTrans: List[((Node, Label, Node), Int)] = l.transRel.rel.zipWithIndex.map(x => (x._1, x._2 + 1))

  //println(indexTrans)
  val mapIndexTrans: Map[Int, (Node, Label, Node)] = indexTrans.map(x => (x._2, x._1)).toMap
  val indexLabel: Map[String, Int] = l.listLab.zipWithIndex.map(x => (x._1, x._2 + 1)).toMap

  // tail[i] = t dove i = (t,l,h) e   1 =< i <= m dove m e' il numero massimo di transizioni


  val tail: Vector[Int] = -1 +: indexTrans.map(x => mapNodeSting getOrElse(x._1._1.name, -1)).toVector
  /*
  val label: Vector[Int] = -1 +: indexTrans.map(x => indexLabel getOrElse(x._1._2.name, -1)).toVector
  val head: Vector[Int] = -1 +: indexTrans.map(x => mapNodeSting getOrElse(x._1._3.name, -1)).toVector
  */

  /*
    val in_trans: Map[Int, List[Int]] = indexTrans.groupBy((x)=> x._1._3.name).toVector
      .map((x )=> (mapNodeSting.getOrElse(x._1,-1),x._2.map(y => y._2))).toMap
  */
  val in_transition = indexTrans
    .groupBy(x => x._1._3.name)
    .toVector
    .map(x => (mapNodeSting.getOrElse(x._1, 0), x._2))
    .map(x => (x._1, x._2.map(y => y._2)))
    .toMap
    //initInTrans()



  def In_transitions(s: Int): List[Int] = {


    //in_trans.getOrElse(s,Nil)
    //indexTrans.filter(x => mapNodeSting.getOrElse(x._1._3.name, -1) == s).map(x => x._2)
    in_transition.getOrElse(s,Nil)
  }


  /*
    val elemOrderSplitter = indexTrans
      .groupBy(x => x._1._2.name)
      .toVector
      .sortBy(x => x._1) //ordinati per label
      .map(x => x._2.sortWith((t1, t2) => {

        if (t1._1._1.name < t2._1._1.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name < t2._1._2.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name == t2._1._2.name && t1._1._3.name < t2._1._3.name)
          true
        else
          false
      }))
      .map(x => x.map(y => y._2).toVector)

    val elemorderoutsets = indexTrans
      .groupBy(x => x._1._2.name)
      .toVector
      .sortBy(x => x._1) //ordinati per label
      .map(x => x._2.sortWith((t1, t2) => {
        if (t1._1._1.name < t2._1._1.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name < t2._1._2.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name == t2._1._2.name && t1._1._3.name < t2._1._3.name)
          true
        else
          false
      }))
      .map(x => x.groupBy(el => el._1._1.name).toVector)
      .map(x => x.sortBy(ord => ord._1))
      .flatMap(y => y.map(x => x._2.sortWith((t1, t2) => {
        if (t1._1._1.name < t2._1._1.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name < t2._1._2.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name == t2._1._2.name && t1._1._3.name < t2._1._3.name)
          true
        else
          false
      }).toVector))
      .map(x => x.map(y => y._2))
  */

  /*
  La seguente funzione (initInTrans) serve
  per inizializzare la struttura per In transitions
  come definita dall'articolo di Valmari
   */
  def initInTrans() = {
    val sets1 = indexTrans
      .groupBy(x => x._1._3.name)
      .toVector
      .map(x => (mapNodeSting.getOrElse(x._1, 0), x._2))
      .map(x => (x._1, x._2.map(y => y._2)))
      .toMap
      //.sortBy(x => x._1)

/*
    val numNodes = l.nodes.length

    val ris = (0 to numNodes).toVector
      .map(x => {
        sets1.getOrElse(x, Nil)
      })


    ris
    */
    sets1

  }


  val raggruppaPerLabel: Vector[List[((Node, Label, Node), Int)]] = indexTrans
    .groupBy(x => x._1._2.name)
    .toVector
    .map(x => x._2)//aggiunto 09 11
    //.sortBy(x => x._1) //ordinati per label
   /* .map(x => x._2.sortWith((t1, t2) => {
      if (t1._1._1.name < t2._1._1.name)
        true
      else if (t1._1._1.name == t2._1._1.name && t1._1._2.name < t2._1._2.name)
        true
      else if (t1._1._1.name == t2._1._1.name && t1._1._2.name == t2._1._2.name && t1._1._3.name < t2._1._3.name)
        true
      else
        false
    }))*/

  def initOutsets(): RefinableStructure = {

/*
    val sets1 = indexTrans
      .groupBy(x => x._1._2.name)
      .toVector
      .sortBy(x => x._1) //ordinati per label
      .map(x => x._2.sortWith((t1, t2) => {
        if (t1._1._1.name < t2._1._1.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name < t2._1._2.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name == t2._1._2.name && t1._1._3.name < t2._1._3.name)
          true
        else
          false
      }))
      .map(x => x.groupBy(el => el._1._1.name).toVector)
      .map(x => x.sortBy(ord => ord._1))
      .flatMap(y => y.map(x => x._2.sortWith((t1, t2) => {
        if (t1._1._1.name < t2._1._1.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name < t2._1._2.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name == t2._1._2.name && t1._1._3.name < t2._1._3.name)
          true
        else
          false
      }).toVector))
      .map(x => x.map(y => y._2))
*/

    /*
    val sets1 = raggruppaPerLabel
      .map(x => x.groupBy(el => el._1._1.name).toVector)
      .map(x => x.sortBy(ord => ord._1))
      .flatMap(y => y.map(x => x._2.sortWith((t1, t2) => {
        if (t1._1._1.name < t2._1._1.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name < t2._1._2.name)
          true
        else if (t1._1._1.name == t2._1._1.name && t1._1._2.name == t2._1._2.name && t1._1._3.name < t2._1._3.name)
          true
        else
          false
      }).toVector))
      .map(x => x.map(y => y._2))
    */

    // aggiornato il 9 11

    val sets1 = raggruppaPerLabel
      .map(x => x.groupBy(el => el._1._1.name).toVector)
      //.map(x => x.sortBy(ord => ord._1))
      .flatMap(y => y.map(x => x._2.toVector))
      .map(x => x.map(y => y._2))


    val elems = 0 +: sets1.flatten

    val loc = elems
      .zipWithIndex
      .sortBy(x => x._1)
      .map(x => x._2)

    val sets = sets1.length

    @tailrec
    def computeFirst(v: Vector[Vector[Int]],
                     ris: Vector[Int],
                     index: Int,
                     bias: Int): Vector[Int] = {

      if ((index > (v.length - 2)))
        ris
      else {
        computeFirst(v, ris :+ (v(index).length + 1 + bias), index + 1,
          bias + v(index).length)
      }

    }


    val firsts = 0 +: computeFirst(sets1, Vector(1), 0, 0)


    val mid1 = firsts
    val ufirst = Vector(0, 1)
    val end = 0 +: (computeFirst(sets1, Vector(1), 0, 0)).tail :+ (sets1.flatten.length + 1)

    val uend = Vector(0, sets1.flatten.length + 1)
    val mid2 = end


    val sidxToUpdate = (0 to (sets1.flatten.size)).toVector

    // funzione per costruire uidx sidx
    @tailrec
    def iterSets(input: Vector[(Vector[Int], Int)],
                 ris: Vector[Int]): Vector[Int] = {

      @tailrec
      def makesetidx(inputf: (Vector[Int], Int),
                     risf: Vector[Int]): Vector[Int] = {

        if (inputf._1.isEmpty)
          risf
        else {
          makesetidx((inputf._1.tail, inputf._2),
            risf.updated(inputf._1.head, inputf._2)
          )
        }

      }

      if (input.isEmpty)
        ris
      else {
        iterSets(input.tail,
          makesetidx(input.head, ris))
      }

    }

    val setsWithIndex = sets1.zipWithIndex.map(x => (x._1, x._2 + 1))

    val sidx = iterSets(setsWithIndex, sidxToUpdate)
    //val uidx = sidx
    val uidx = (0 to sets).toVector.map(_ => 1)








    /*
        val provaStampa = sets1.flatten.map(x => mapIndexTrans.getOrElse(x,
          (new Node("Err"),
          new Label("Err"),
        new Node("Errore")
          )
        )
        )
        provaStampa.map(x => x.toString()).mkString(",")
        */


    new RefinableStructure(
      elems,
      loc,
      firsts,
      mid1,
      mid2,
      end,
      ufirst,
      uend,
      sidx,
      uidx,
      sets,
      bunches = 1
    )


  }


  def initSplitter(): RefinableStructure = {


    val sets1 = raggruppaPerLabel
      .map(x => x.map(y => y._2).toVector)


    val elems = 0 +: sets1.flatten

    val loc = elems
      .zipWithIndex
      .sortBy(x => x._1)
      .map(x => x._2)

    val sets = sets1.length

    @tailrec
    def computeFirst(v: Vector[Vector[Int]],
                     ris: Vector[Int],
                     index: Int,
                     bias: Int): Vector[Int] = {

      if ((index > (v.length - 2)))
        ris
      else {
        computeFirst(v, ris :+ (v(index).length + 1 + bias), index + 1,
          bias + v(index).length)
      }

    }

    val firsts = 0 +: computeFirst(sets1, Vector(1), 0, 0)


    val mid1 = firsts
    val ufirst = mid1
    val end = 0 +: (computeFirst(sets1, Vector(1), 0, 0)).tail :+ (sets1.flatten.length + 1)

    val uend = end
    val mid2 = uend


    val sidxToUpdate = (0 to (sets1.flatten.size)).toVector

    // funzione per costruire uidx sidx
    @tailrec
    def iterSets(input: Vector[(Vector[Int], Int)],
                 ris: Vector[Int]): Vector[Int] = {

      @tailrec
      def makesetidx(inputf: (Vector[Int], Int),
                     risf: Vector[Int]): Vector[Int] = {

        if (inputf._1.isEmpty)
          risf
        else {
          makesetidx((inputf._1.tail, inputf._2),
            risf.updated(inputf._1.head, inputf._2)
          )
        }

      }

      if (input.isEmpty)
        ris
      else {
        iterSets(input.tail,
          makesetidx(input.head, ris))
      }

    }

    val setsWithIndex = sets1.zipWithIndex.map(x => (x._1, x._2 + 1))

    val sidx = iterSets(setsWithIndex, sidxToUpdate)
    //val uidx = sidx
    val uidx = (0 to sets).toVector








    /*
        val provaStampa = sets1.flatten.map(x => mapIndexTrans.getOrElse(x,
          (new Node("Err"),
          new Label("Err"),
        new Node("Errore")
          )
        )
        )
        provaStampa.map(x => x.toString()).mkString(",")
        */


    new RefinableStructure(
      elems,
      loc,
      firsts,
      mid1,
      mid2,
      end,
      ufirst,
      uend,
      sidx,
      uidx,
      sets,
      bunches = sets
    )

  }


  def printSet(r: RefinableStructure): String = {

    val toPrint = (for {
      b <- 1 to r.bunches
      s <- 1 to r.sets
      if (r.Bunch(s) == b)
      e <- r.elems
      if (r.Set(e) == s)
      m = if (r.loc(e) >= r.mid1(s)
        && r.loc(e) < r.mid2(s)) 3 else if (r.loc(e) < r.mid1(s)) 1 else 2
    } yield (e, s, b, m)).toList


    val listBunches = toPrint.groupBy(x => x._3)


    def mapToPrint(listToPrint: List[(Int, Int, Int, Int)]) = {

      val mapToPrintRis = listToPrint.groupBy(x => x._2).map(x => {
        val str1 = "Set "
          .concat(x._1.toString)
          .concat(" = { ")
        val listIter = x._2
        val listUnmarked = listIter.filter(x => x._4 == 3).map(x => mapNodeInt getOrElse(x._1, "Errore"))
        val listmark1 = listIter.filter(x => x._4 == 1).map(x => mapNodeInt getOrElse(x._1, "Errore"))
        val listmark2 = listIter.filter(x => x._4 == 2).map(x => mapNodeInt getOrElse(x._1, "Errore"))
        val str2 = "Unmarked = { " ++
          listUnmarked.mkString(",") ++ " } ; " ++
          " Mark 1 = { " ++ listmark1.mkString(",") ++ "} ; " ++
          " Mark 2 = { " ++ listmark2.mkString(",") ++ "} "
        /*val str2 = x._2.map(f => f._1.toString)
          .mkString(",")
          .concat(" }")*/
        str1 ++ str2 ++ "}"
      }).mkString("\n")

      mapToPrintRis

    }

    val states = listBunches
      .toList
      .sortBy(x => x._1)
      .map(x => {
        val str1 = "Bunches "
          .concat(x._1.toString)
          .concat("= { \n")
        val list1 = x._2
        val str2 = mapToPrint(list1)
          .concat("\n}")
        //.mkString("\n")
        (str1 ++ str2)
      })


    states.mkString("\n")


  }


  def printSetOfTransition(r: RefinableStructure): String = {


    val toPrint = (for {
      b <- 1 to r.bunches
      s <- 1 to r.sets
      if (r.Bunch(s) == b)
      e <- r.elems
      if (r.Set(e) == s)
      m = if (r.loc(e) >= r.mid1(s)
        && r.loc(e) < r.mid2(s)) 3 else if (r.loc(e) < r.mid1(s)) 1 else 2
    } yield (e, s, b, m)).toList


    val listBunches = toPrint.groupBy(x => x._3)


    def mapToPrint(listToPrint: List[(Int, Int, Int, Int)]) = {

      val mapToPrintRis = listToPrint.groupBy(x => x._2).map(x => {
        val str1 = "Set "
          .concat(x._1.toString)
          .concat(" = { ")
        val listIter = x._2
        val listUnmarked = listIter.filter(x => x._4 == 3).map(x => mapIndexTrans.getOrElse(x._1, (
          new Node("Errore"), new Label("Errore"), new Node("Errore")
        )).toString())
        val listmark1 = listIter.filter(x => x._4 == 1).map(x => mapIndexTrans.getOrElse(x._1, (
          new Node("Errore"), new Label("Errore"), new Node("Errore")
        )).toString())
        val listmark2 = listIter.filter(x => x._4 == 2).map(x => mapIndexTrans.getOrElse(x._1, (
          new Node("Errore"), new Label("Errore"), new Node("Errore")
        )).toString())
        val str2 = "Unmarked = { " ++
          listUnmarked.mkString(",") ++ " } ; " ++
          " Mark 1 = { " ++ listmark1.mkString(",") ++ "} ; " ++
          " Mark 2 = { " ++ listmark2.mkString(",") ++ "} "
        /*val str2 = x._2.map(f => f._1.toString)
          .mkString(",")
          .concat(" }")*/
        str1 ++ str2 ++ "}"
      }).mkString("\n")

      mapToPrintRis

    }

    val states = listBunches
      .toList
      .sortBy(x => x._1)
      .map(x => {
        val str1 = "Bunches "
          .concat(x._1.toString)
          .concat("= { \n")
        val list1 = x._2
        val str2 = mapToPrint(list1)
          .concat("\n}")
        //.mkString("\n")
        (str1 ++ str2)
      })


    states.mkString("\n")


  }


  def initBlock(): RefinableStructure = {


    val items = l.nodes.length
    val elems = (0 to items).toVector
    val loc = elems
    val first = Vector(0, 1)
    val end = Vector(0, items + 1)
    val mid1 = first
    val mid2 = end
    val ufirst = first
    val uend = end
    val sidx = elems.map(_ => 1).updated(0, 0)
    val uidx = sidx
    val sets = 1
    val bunches = 1
    new RefinableStructure(
      elems,
      loc,
      first,
      mid1,
      mid2,
      end,
      ufirst,
      uend,
      sidx,
      uidx,
      sets,
      bunches)

  }


}

