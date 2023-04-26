package lazabs.horn.symex_gnn

import ap.terfor.conjunctions.Conjunction
import lazabs.GlobalParameters
import lazabs.horn.bottomup.{AbstractState, HornClauses, NormClause, RelationSymbol}
import lazabs.horn.symex.UnitClause

import scala.collection.mutable.{PriorityQueue, Queue => MQueue}
import java.io.{File, PrintWriter}
import play.api.libs.json.{JsSuccess, JsValue, Json}

object HornGraphType extends Enumeration {
  val CDHG, CG = Value
}

trait StateQueue {
  type ChoiceQueueElement = (NormClause, Seq[UnitClause])
  def isEmpty: Boolean
  def size: Int
  def enqueue(e:(NormClause,Seq[UnitClause])): Unit
  def dequeue(): (NormClause,Seq[UnitClause])
}

class PriorityChoiceQueue(normClauseToScore: Map[NormClause, Double]) extends StateQueue {
  //type ChoiceQueueElement = (NormClause, Seq[UnitClause])
  val coefClauseScoreFromGNN = 1000
  //println(Console.BLUE+"ChoiceQueue:PriorityChoiceQueue")
  private def priority(s: ChoiceQueueElement) = {
    val (nc, ucs) = s
    val normclauseSocre = normClauseToScore(nc)
    val unitClauseSeqScore = ucs.map(_.constraint.size).sum //+ nc._2.map(_.rs.arity).sum
    val queueElementScore = normclauseSocre * coefClauseScoreFromGNN + unitClauseSeqScore
    -queueElementScore.toInt
  }
  private implicit val ord = new Ordering[ChoiceQueueElement] {
    def compare(s: ChoiceQueueElement, t: ChoiceQueueElement) =
      priority(t) - priority(s)
  }

  private val states = new PriorityQueue[ChoiceQueueElement]
  def isEmpty: Boolean =
    states.isEmpty
  def size: Int =
    states.size
  def enqueue(e:(NormClause,Seq[UnitClause])): Unit = {
    //println(Console.BLUE+"enqueue",e._1,e._2)
    states += ((e._1, e._2))
  }

  def dequeue(): (NormClause, Seq[UnitClause]) = {
    val (nc, ucs) = states.dequeue
    (nc, ucs)
  }
}

object clausePriorityGNN {
  val coefClauseScoreFromGNN = 1000

  def prioritizeQueue(choiceQueue: MQueue[(NormClause, Seq[UnitClause])], normClauseToScore: Map[NormClause, Double]): Unit = {
    //extract elements from choiceQueue
    //for (e<-choiceQueue) println(Console.BLUE + e._1 + " " + e._2)
    val queueSeq = (for (i <- 1 to choiceQueue.length) yield choiceQueue.dequeue()).toSeq

    //sort elements by score
    val queueSeqToScore = for (nc <- queueSeq) yield {
      val normclauseSocre = normClauseToScore(nc._1)
      val unitClauseSeqScore = nc._2.map(_.constraint.size).sum //+ nc._2.map(_.rs.arity).sum
      val queueElementScore = normclauseSocre * coefClauseScoreFromGNN + unitClauseSeqScore

      if (GlobalParameters.get.log) {
        println(Console.YELLOW_B + " normClause constraint size:" + nc._1.constraint.size + " score:" + normclauseSocre)
        println(Console.YELLOW + " Seq[UnitClause].length:" + nc._2.length)
        println(Console.YELLOW + "Seq[UnitClause].head.constraint size:" + nc._2.head.constraint.size)
        println(Console.YELLOW + "Seq[UnitClause].head.constraint predicate size:" + nc._2.head.constraint.predicates.size)
        println(Console.YELLOW + "Seq[UnitClause].head.constraint variable size:" + nc._2.head.constraint.variables.size)
        println(Console.YELLOW + "Seq[UnitClause].head.rs arity:" + nc._2.head.rs.arity)
        //UnitClause.constraint.constants.size = UnitClause.constraint.size+1
      }

      (nc, queueElementScore)
    }
    val sortedQueueSeqToScore = queueSeqToScore.sortBy(_._2).reverse

    //for (e<-queueSeqToScore) println(Console.YELLOW + e._1 + " " + e._2)
    //for (e<-sortedQueueSeqToScore) println(Console.YELLOW_B + e._1 + " " + e._2)

    //enqueue sorted elements to choiceQueue
    for (s <- sortedQueueSeqToScore) choiceQueue.enqueue(s._1)
    //for (e<-choiceQueue) println(Console.RED + e._1 + " " + e._2)

    if (GlobalParameters.get.log) {
      println(Console.BLUE + "queueSeq length:" + queueSeq.size)
      println(Console.BLUE + "queueSeqToScore length:" + queueSeqToScore.size)
      println(Console.BLUE + "sortedQueueSeqToScore length:" + sortedQueueSeqToScore.size)
      println(Console.BLUE + "choiceQueue length:" + choiceQueue.size)
    }

    //sys.exit(0)


  }

  def prioritizeClauses(normClauses: Seq[NormClause], normClauseToScore: Map[NormClause, Double]): Seq[NormClause] = {

    val currentNormClauseToScore = for (nc <- normClauses) yield (nc, normClauseToScore(nc))
    //val sortedCurrentNormClauseToScore = currentNormClauseToScore.sortBy(_._2)
    val sortedCurrentNormClauseToScore = currentNormClauseToScore.sortBy(_._2).reverse

    // print middle data for debug
    println(Console.BLUE + "normClauseToScore length:" + normClauseToScore.size)
    println(Console.BLUE + "currentNormClauseToScore length:" + currentNormClauseToScore.size)
    //for (c<-currentNormClauseToScore) println(Console.BLUE + c._1 + " " + c._2)
    //for (c<-sortedCurrentNormClauseToScore) println(Console.YELLOW + c._1 + " " + c._2)


    sortedCurrentNormClauseToScore.map(_._1)
    //normClauses
  }


  def readClauseScores[CC](clauses: Iterable[CC]): Map[CC, Double] = {
    //get graph file name
    val graphFileName =
      if (GlobalParameters.get.fileName.contains("simplified"))
        GlobalParameters.get.fileName.stripSuffix(".simplified.smt2") + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
      else
        GlobalParameters.get.fileName + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
    //read logit values from graph file
    val predictedLogitsFromGraph = readJsonFieldDouble(graphFileName, readLabelName = "predictedLabelLogit")
    //for CDHG map predicted (read) Logits to correct clause number, for CG just return predictedLogitsFromGraph
    val predictedLogits = GlobalParameters.get.hornGraphType match {
      case HornGraphType.CDHG => {
        val labelMask = readJsonFieldInt(graphFileName, readLabelName = "labelMask")
        val originalClausesIndex = labelMask.distinct
        val separatedPredictedLabels = for (i <- originalClausesIndex) yield {
          for (ii <- (0 until labelMask.count(_ == i))) yield predictedLogitsFromGraph(i + ii)
        }
        val logitsForOriginalClauses = for (sl <- separatedPredictedLabels) yield {
          sl.max
        }
        logitsForOriginalClauses
      }
      case HornGraphType.CG => {
        predictedLogitsFromGraph
      }
    }

    //println(Console.BLUE + "predictedLogits length:" + predictedLogits.length)
    (for ((c, s) <- clauses.zip(predictedLogits)) yield (c, s)).toMap
  }

  def graphFileNameMap(hgt: HornGraphType.Value): String = hgt match {
    case HornGraphType.CDHG => "hyperEdgeGraph"
    case HornGraphType.CG => "monoDirectionLayerGraph"
  }

  def readJsonFieldDouble(fileName: String, readLabelName: String): Array[Double] = {
    val json_data = readJSONFile(fileName)
    val readLabel = (json_data \ readLabelName).validate[Array[Double]] match {
      case JsSuccess(templateLabel, _) => templateLabel
    }
    readLabel
  }

  def readJsonFieldInt(fileName: String, readLabelName: String): Array[Int] = {
    val json_data = readJSONFile(fileName)
    val readLabel = (json_data \ readLabelName).validate[Array[Int]] match {
      case JsSuccess(templateLabel, _) => templateLabel
    }
    readLabel
  }

  def readJSONFile(fileName: String): JsValue = {
    val json_content = scala.io.Source.fromFile(fileName).mkString
    Json.parse(json_content)
  }


}
